open Notty
open Notty.Infix

type ui = {
  mutable sel : int;
  mutable sel_id : string option;  (** keeps the selection on the same job as the list changes *)
  mutable scroll : int;
  mutable confirm_kill : Model.job option;
}

let create () = { sel = 0; sel_id = None; scroll = 0; confirm_kill = None }

let bold = A.st A.bold
let dim = A.fg A.(gray 12)
let sel = A.st A.reverse
let red = A.fg A.lightred
let green = A.fg A.lightgreen
let yellow = A.fg A.lightyellow
let cyan = A.fg A.lightcyan

(* exactly [w] columns *)
let text ?(attr = A.empty) w s = I.hsnap ~align:`Left (max 0 w) (I.string attr s)
let rtext ?(attr = A.empty) w s = I.hsnap ~align:`Right (max 0 w) (I.string attr s)

let columns = [ ("zone", 10, false); ("def", 11, false); ("up", 7, true); ("last", 6, true); ("state", 9, false) ]
let fixed_w = List.fold_left (fun acc (_, w, _) -> acc + w + 1) 0 columns

let row ?(attr = A.empty) w cells progress =
  let cells = List.map2 (fun (_, cw, right) c -> if right then rtext ~attr cw c else text ~attr cw c) columns cells in
  I.hcat (List.map (fun i -> i <|> I.string attr " ") cells) <|> text ~attr (w - fixed_w) progress

let state t ~now (j : Model.job) =
  if j.killing then ("KILLING", A.(st bold ++ fg lightred))
  else if not (Model.active j) then
    ( (match j.status with "ENDED" -> "ended" | s -> String.lowercase_ascii s),
      match j.status with "SUCCEEDED" -> green | "FAILED" -> red | _ -> dim )
  else if Model.stalled t ~now j then ("STALLED", A.(st bold ++ fg lightred))
  else if Model.hung j then ("HUNG", yellow)
  else if j.status = "STARTING" then ("starting", dim)
  else if j.last_event = None then ("booting", dim)
  else ("ok", green)

let job_row t ~now ui w k (j : Model.job) =
  let st, st_attr = state t ~now j in
  let clock = Model.clock ~now j in
  let up = match j.started with Some s -> Aws.duration (clock -. s) | None -> "-" in
  let last = match j.last_event with Some e -> Aws.duration (clock -. e) | None -> "-" in
  let progress = if j.progress <> "" then j.progress else if Model.active j then "<booting / no progress yet>" else j.reason in
  let base = if Model.stalled t ~now j then red else if not (Model.active j) then dim else A.empty in
  let attr = if k = ui.sel then sel else base in
  let st_attr = if k = ui.sel then sel else st_attr in
  let cells = List.map2 (fun (_, cw, right) c -> if right then rtext ~attr cw c else text ~attr cw c) (List.filteri (fun i _ -> i < 4) columns) [ j.zy; j.def; up; last ] in
  I.hcat (List.map (fun i -> i <|> I.string attr " ") cells) <|> text ~attr:st_attr 9 st <|> I.string attr " " <|> text ~attr (w - fixed_w) progress

let log_pane (j : Model.job option) ui ~now w h =
  match j with
  | None -> I.vsnap ~align:`Top h (text ~attr:dim w "no jobs")
  | Some j ->
      let lines = Array.of_seq (Queue.to_seq j.log) in
      let n = Array.length lines in
      let h' = max 0 (h - 2) in
      ui.scroll <- max 0 (min ui.scroll (n - h'));
      let last = n - ui.scroll in
      let first = max 0 (last - h') in
      let body = I.vcat (List.init (last - first) (fun k -> text w lines.(first + k))) in
      let last_ev =
        match j.last_event with
        | Some e when Model.active j -> Printf.sprintf "last output %s ago" (Aws.duration (now -. e))
        | Some e -> Printf.sprintf "last output %s before it stopped" (Aws.duration (Model.clock ~now j -. e))
        | None -> "no output seen"
      in
      let scroll = if ui.scroll > 0 then Printf.sprintf "  [-%d lines]" ui.scroll else "" in
      let title = Printf.sprintf "%s  %s  job %s  %s%s" j.name j.status (String.sub j.id 0 (min 8 (String.length j.id))) last_ev scroll in
      let sub = Printf.sprintf "%s  %s%s" j.def (Option.value j.stream ~default:"(no log stream yet)") (if j.reason <> "" then "  " ^ j.reason else "") in
      text ~attr:bold w title <-> text ~attr:dim w sub <-> I.vsnap ~align:`Top h' body

let render (t : Model.t) ui (w, h) =
  let now = Unix.gettimeofday () in
  let rows = Array.of_list (Model.rows t) in
  (match ui.sel_id with
  | Some id -> ( match Array.find_index (fun (j : Model.job) -> j.id = id) rows with Some k -> ui.sel <- k | None -> ())
  | None -> ());
  ui.sel <- max 0 (min ui.sel (Array.length rows - 1));
  let running, starting, stalled = Model.counts t ~now in
  let header =
    I.string bold ("fargate " ^ t.cfg.prefix)
    <|> I.string A.empty (Printf.sprintf "   queue %s   running %d   starting %d   queued %d   " t.cfg.queue running starting t.queued)
    <|> I.string (if stalled > 0 then A.(st bold ++ fg lightred) else A.empty) (Printf.sprintf "stalled %d" stalled)
    <|> I.string dim (Printf.sprintf "   stall >%.0fs   up %s" t.cfg.stall (Aws.duration (now -. t.started_at)))
    <|> (if t.refreshing then I.string dim "   refreshing" else I.string dim (Printf.sprintf "   next poll %s" (Aws.duration (t.cfg.refresh -. (now -. t.last_refresh)))))
    <|> if t.tail = None then I.string yellow "   NOT TAILING" else I.string dim (Printf.sprintf "   tailing %d" (List.length (snd (Option.get t.tail))))
  in
  let footer =
    match ui.confirm_kill with
    | Some j -> I.string A.(st bold ++ fg lightyellow) (Printf.sprintf "Kill %s (%s)? (y/n)" j.name j.status)
    | None -> I.string dim "↑↓ select  K/Del kill  PgUp/Dn End log  r refresh  q quit" <|> I.string yellow (if t.message = "" then "" else "   " ^ t.message)
  in
  let mid_h = max 0 (h - 3) in
  let left_w = max (min w (fixed_w + 28)) (min (w - 40) (w * 55 / 100)) in
  let right_w = max 0 (w - left_w - 1) in
  let list_h = max 0 (mid_h - 1) in
  let first = max 0 (ui.sel - list_h + 1) in
  let visible = Array.to_list (Array.sub rows first (max 0 (min list_h (Array.length rows - first)))) in
  let head = row ~attr:(A.st A.underline) left_w (List.map (fun (c, _, _) -> c) columns) "progress" in
  let list = I.vcat (List.mapi (fun k j -> job_row t ~now ui left_w (first + k) j) visible) in
  let left = head <-> I.vsnap ~align:`Top list_h list in
  let middle = I.hsnap ~align:`Left left_w left <|> I.char dim '|' 1 mid_h <|> log_pane (if Array.length rows = 0 then None else Some rows.(ui.sel)) ui ~now right_w mid_h in
  header <-> I.void 1 1 <-> I.vsnap ~align:`Top mid_h middle <-> footer |> I.hsnap ~align:`Left w

(* Returns true to quit. *)
let handle (t : Model.t) ui (key : Unescape.key) =
  let rows = Array.of_list (Model.rows t) in
  let selected () = if Array.length rows = 0 then None else Some rows.(max 0 (min ui.sel (Array.length rows - 1))) in
  let move d =
    ui.scroll <- 0;
    ui.sel <- max 0 (min (Array.length rows - 1) (ui.sel + d));
    ui.sel_id <- Option.map (fun (j : Model.job) -> j.id) (selected ())
  in
  match ui.confirm_kill with
  | Some j ->
      ui.confirm_kill <- None;
      (match key with `ASCII ('y' | 'Y'), _ -> Model.kill t j | _ -> t.message <- "kill cancelled");
      false
  | None -> (
      match key with
      | `ASCII ('q' | 'Q'), [] | `ASCII 'C', [ `Ctrl ] -> true
      | `Arrow `Up, _ | `ASCII 'k', [] -> move (-1); false
      | `Arrow `Down, _ | `ASCII 'j', [] -> move 1; false
      | `Home, _ -> move (-Array.length rows); false
      | `Page `Up, _ -> ui.scroll <- ui.scroll + 10; false
      | `Page `Down, _ -> ui.scroll <- max 0 (ui.scroll - 10); false
      | `End, _ -> ui.scroll <- 0; false
      | `ASCII 'r', [] ->
          Model.force_refresh t;
          t.message <- "refreshing";
          false
      | `ASCII 'K', _ | `Delete, _ ->
          (match selected () with
          | Some j when Model.active j && not j.killing -> ui.confirm_kill <- Some j
          | Some j when j.killing -> t.message <- j.name ^ ": kill already requested"
          | Some j -> t.message <- j.name ^ " is not active"
          | None -> ());
          false
      | _ -> false)
