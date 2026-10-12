(* The state behind the screen: the jobs in the queue, one tail over all
   their log streams, and the short aws commands in flight. Everything is
   driven from [pump], called from the main loop; nothing blocks. *)

type job = {
  name : string;
  zy : string;  (** name without the prefix *)
  id : string;
  mutable status : string;  (** RUNNING, STARTING, ...; ENDED until describe-jobs says *)
  mutable started : float option;
  mutable stopped : float option;
  mutable stream : string option;
  mutable stream_ok : bool;  (** the stream exists, so it may be tailed *)
  mutable checking : bool;  (** a describe-log-streams for it is in flight *)
  mutable check_after : float;  (** no new check before this *)
  mutable def : string;
  mutable reason : string;
  first_seen : float;
  mutable ended : float option;  (** when it left the active list *)
  mutable last_event : float option;  (** timestamp of the newest log event *)
  mutable last_msg : string;
  mutable progress : string;
  mutable killing : bool;
  log : string Queue.t;
}

type cmd = {
  proc : Proc.t;
  out : string list ref;  (** lines, newest first *)
  k : int -> string list -> unit;
}

type t = {
  cfg : Aws.cfg;
  env : string array;
  jobs : (string, job) Hashtbl.t;  (** by id *)
  by_stream : (string, job) Hashtbl.t;
  mutable cmds : cmd list;
  mutable refreshing : bool;
  mutable last_refresh : float;
  mutable describing : bool;
  mutable describe_after : float;
  mutable tail : (Proc.t * string list) option;
  mutable tail_after : float;  (** no (re)start before this *)
  mutable tail_error : string;
  mutable queued : int;
  mutable message : string;
  started_at : float;
}

let max_log = 2000
let keep_ended = 15. *. 60.

let create cfg =
  {
    cfg;
    env = Aws.env cfg;
    jobs = Hashtbl.create 64;
    by_stream = Hashtbl.create 64;
    cmds = [];
    refreshing = false;
    last_refresh = 0.;
    describing = false;
    describe_after = 0.;
    tail = None;
    tail_after = 0.;
    tail_error = "";
    queued = 0;
    message = "";
    started_at = Unix.gettimeofday ();
  }

let active j = j.ended = None
let running j = active j && j.status = "RUNNING"

(* the latest moment we know the job was alive *)
let activity j = List.fold_left (fun acc o -> match o with Some v when v > acc -> v | _ -> acc) j.first_seen [ j.last_event; j.started ]
let idle_for ~now j = now -. activity j

(* the moment a job's clocks stop: now while it runs, otherwise when it stopped *)
let clock ~now j = if active j then now else match (j.stopped, j.ended) with Some s, _ -> s | None, Some e -> e | None, None -> now
let stalled t ~now j = running j && idle_for ~now j > t.cfg.stall
let hung j = active j && (try ignore (Str.search_forward (Str.regexp_string "0.0/s") j.progress 0); true with Not_found -> false)

(* active jobs by zone and year, then the ended ones, newest first *)
let rows t =
  let all = Hashtbl.fold (fun _ j acc -> j :: acc) t.jobs [] in
  let act, ended = List.partition active all in
  List.sort (fun a b -> compare (Aws.zone_key a.name) (Aws.zone_key b.name)) act
  @ List.sort (fun a b -> compare b.ended a.ended) ended

let counts t ~now =
  Hashtbl.fold
    (fun _ j (r, s, st) -> if running j then (r + 1, s, if stalled t ~now j then st + 1 else st) else if active j then (r, s + 1, st) else (r, s, st))
    t.jobs (0, 0, 0)

(* ---- short commands ---- *)

let run t argv k = t.cmds <- { proc = Proc.spawn ~env:t.env argv; out = ref []; k } :: t.cmds

let add_line j line =
  Queue.add line j.log;
  if Queue.length j.log > max_log then ignore (Queue.pop j.log)

let new_job t ~now name id =
  let zy = if String.starts_with ~prefix:t.cfg.prefix name then String.sub name (String.length t.cfg.prefix) (String.length name - String.length t.cfg.prefix) else name in
  {
    name;
    zy;
    id;
    status = "";
    started = None;
    stopped = None;
    stream = None;
    stream_ok = false;
    checking = false;
    check_after = 0.;
    def = "?";
    reason = "";
    first_seen = now;
    ended = None;
    last_event = None;
    last_msg = "";
    progress = "";
    killing = false;
    log = Queue.create ();
  }

let apply_listing t (rows : (string * string * string * string) list) =
  let now = Unix.gettimeofday () in
  let seen = Hashtbl.create 32 in
  List.iter
    (fun (status, name, id, started) ->
      if status = "RUNNING" || status = "STARTING" then begin
        Hashtbl.replace seen id ();
        let j =
          match Hashtbl.find_opt t.jobs id with
          | Some j -> j
          | None ->
              let j = new_job t ~now name id in
              Hashtbl.replace t.jobs id j;
              j
        in
        j.status <- status;
        j.ended <- None;
        if j.started = None then j.started <- Aws.ms started
      end)
    rows;
  t.queued <- List.length (List.filter (fun (st, _, _, _) -> st = "RUNNABLE") rows);
  Hashtbl.iter
    (fun id j ->
      if active j && not (Hashtbl.mem seen id) then begin
        j.ended <- Some now;
        j.status <- "ENDED"
      end)
    t.jobs;
  t.describe_after <- 0.

let refresh t =
  t.refreshing <- true;
  t.last_refresh <- Unix.gettimeofday ();
  let statuses = [ "RUNNING"; "STARTING"; "RUNNABLE" ] in
  let acc = ref [] and left = ref (List.length statuses) and failed = ref false in
  List.iter
    (fun status ->
      run t (Aws.list_jobs t.cfg status) (fun rc lines ->
          if rc <> 0 then begin
            failed := true;
            t.message <- Printf.sprintf "list-jobs %s: %s" status (match lines with l :: _ -> Proc.clean l | [] -> "exit " ^ string_of_int rc)
          end
          else
            List.iter
              (fun l -> match Aws.fields l with name :: id :: started :: _ -> acc := (status, name, id, started) :: !acc | _ -> ())
              lines;
          decr left;
          if !left = 0 then begin
            t.refreshing <- false;
            if not !failed then apply_listing t !acc
          end))
    statuses

let describe t =
  let now = Unix.gettimeofday () in
  let need = Hashtbl.fold (fun _ j acc -> if (active j && j.stream = None) || (not (active j) && j.status = "ENDED") then j.id :: acc else acc) t.jobs [] in
  if need <> [] && (not t.describing) && now >= t.describe_after then begin
    t.describing <- true;
    t.describe_after <- now +. 5.;
    run t (Aws.describe_jobs need) (fun rc lines ->
        t.describing <- false;
        if rc <> 0 then t.message <- "describe-jobs: " ^ (match lines with l :: _ -> Proc.clean l | [] -> "exit " ^ string_of_int rc)
        else
          List.iter
            (fun l ->
              match Aws.fields l with
              | id :: status :: stream :: def :: started :: stopped :: rest -> (
                  match Hashtbl.find_opt t.jobs id with
                  | None -> ()
                  | Some j ->
                      if not (active j) then j.status <- status;
                      (match Aws.opt stream with
                      | Some s when j.stream = None ->
                          j.stream <- Some s;
                          Hashtbl.replace t.by_stream s j
                      | _ -> ());
                      (match Aws.opt def with Some d -> j.def <- (match String.rindex_opt d '/' with Some i -> String.sub d (i + 1) (String.length d - i - 1) | None -> d) | None -> ());
                      if j.started = None then j.started <- Aws.ms started;
                      j.stopped <- Aws.ms stopped;
                      j.reason <- Option.value (Aws.opt (String.concat " " rest)) ~default:"")
              | _ -> ())
            lines)
  end

(* confirm that newly learned streams exist before tailing them *)
let check_streams t =
  let now = Unix.gettimeofday () in
  Hashtbl.iter
    (fun _ j ->
      match j.stream with
      | Some s when active j && (not j.stream_ok) && (not j.checking) && now >= j.check_after ->
          j.checking <- true;
          run t (Aws.stream_exists t.cfg s) (fun rc lines ->
              j.checking <- false;
              j.check_after <- Unix.gettimeofday () +. 10.;
              if rc = 0 && List.exists (fun l -> List.mem s (Aws.fields l)) lines then j.stream_ok <- true)
      | _ -> ())
    t.jobs

(* ---- the tail ---- *)

let on_event t ts stream msg =
  match Hashtbl.find_opt t.by_stream stream with
  | None -> ()
  | Some j ->
      let fresh = match j.last_event with None -> true | Some l -> ts > l || (ts = l && msg <> j.last_msg) in
      if fresh then begin
        j.last_event <- Some ts;
        j.last_msg <- msg;
        let text = String.trim (Proc.clean msg) in
        add_line j (Aws.hms ts ^ " " ^ text);
        match Aws.progress_of text with Some p -> j.progress <- p | None -> ()
      end

(* "<iso timestamp> <stream> <message>" *)
let on_tail_line t raw =
  let raw = Proc.strip_escapes raw in
  let parsed =
    match String.index_opt raw ' ' with
    | Some i when i >= 19 && String.length raw > 10 && raw.[10] = 'T' -> (
        let rest = String.sub raw (i + 1) (String.length raw - i - 1) in
        let stream, msg = match String.index_opt rest ' ' with Some k -> (String.sub rest 0 k, String.sub rest (k + 1) (String.length rest - k - 1)) | None -> (rest, "") in
        match Aws.epoch_of_iso (String.sub raw 0 i) with Some ts -> Some (ts, stream, msg) | None -> None)
    | _ -> None
  in
  match parsed with
  | Some (ts, stream, msg) -> on_event t ts stream msg
  | None -> if String.trim raw <> "" then t.tail_error <- Proc.clean raw

let wanted_streams t = Hashtbl.fold (fun _ j acc -> match j.stream with Some s when active j && j.stream_ok -> s :: acc | _ -> acc) t.jobs [] |> List.sort compare

let manage_tail t =
  let now = Unix.gettimeofday () in
  let wanted = wanted_streams t in
  let current = match t.tail with Some (_, s) -> s | None -> [] in
  if wanted <> current && now >= t.tail_after then begin
    (match t.tail with
    | Some (p, _) ->
        Proc.kill p;
        Proc.close p;
        ignore (Proc.poll p)
    | None -> ());
    t.tail <- None;
    t.tail_after <- now +. 5.;
    if wanted <> [] then t.tail <- Some (Proc.spawn ~env:t.env (Aws.tail t.cfg wanted), wanted)
  end

let pump_tail t =
  match t.tail with
  | None -> ()
  | Some (p, _) -> (
      Proc.read p (on_tail_line t);
      match Proc.poll p with
      | None -> ()
      | Some rc ->
          Proc.close p;
          t.tail <- None;
          t.message <- Printf.sprintf "log tail exited (%d): %s" rc t.tail_error;
          t.tail_after <- Unix.gettimeofday () +. 5.)

(* ---- actions ---- *)

let kill t j =
  j.killing <- true;
  let argv = if j.status = "RUNNING" || j.status = "STARTING" then Aws.terminate_job j.id else Aws.cancel_job j.id in
  run t argv (fun rc lines ->
      t.message <-
        (if rc = 0 then Printf.sprintf "%s: %s requested" j.name (if argv.(2) = "terminate-job" then "terminate" else "cancel")
         else Printf.sprintf "%s: kill failed: %s" j.name (match lines with l :: _ -> Proc.clean l | [] -> "exit " ^ string_of_int rc));
      t.last_refresh <- 0.)

let force_refresh t = t.last_refresh <- 0.

(* ---- the loop's hooks ---- *)

let pump t =
  let now = Unix.gettimeofday () in
  if (not t.refreshing) && now -. t.last_refresh >= t.cfg.refresh then refresh t;
  t.cmds <-
    List.filter
      (fun c ->
        Proc.read c.proc (fun l -> c.out := l :: !(c.out));
        match Proc.poll c.proc with
        | None -> true
        | Some rc ->
            Proc.close c.proc;
            c.k rc (List.rev !(c.out));
            false)
      t.cmds;
  pump_tail t;
  describe t;
  check_streams t;
  manage_tail t;
  Hashtbl.filter_map_inplace
    (fun _ j ->
      match j.ended with
      | Some at when now -. at > keep_ended ->
          Option.iter (Hashtbl.remove t.by_stream) j.stream;
          None
      | _ -> Some j)
    t.jobs

let fds t = List.map (fun c -> c.proc.Proc.fd) t.cmds @ match t.tail with Some (p, _) -> [ p.Proc.fd ] | None -> []

let shutdown t =
  List.iter (fun c -> Proc.kill c.proc; Proc.close c.proc) t.cmds;
  (match t.tail with Some (p, _) -> Proc.kill p; Proc.close p | None -> ());
  t.cmds <- [];
  t.tail <- None
