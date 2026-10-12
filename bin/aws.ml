(* Command lines for the aws CLI, and parsers for its text output. *)

type cfg = {
  region : string;
  profile : string option;
  queue : string;
  group : string;
  prefix : string;
  stall : float;  (** seconds without log output before a running job is STALLED *)
  since : string;  (** how far back a (re)started tail reads, e.g. "15m" *)
  refresh : float;  (** seconds between list-jobs polls *)
}

let env cfg =
  let ours = [ "AWS_DEFAULT_REGION"; "AWS_PAGER"; "AWS_PROFILE" ] in
  let key kv = match String.index_opt kv '=' with Some i -> String.sub kv 0 i | None -> kv in
  let keep = List.filter (fun kv -> not (List.mem (key kv) ours)) (Array.to_list (Unix.environment ())) in
  let extra = [ "AWS_DEFAULT_REGION=" ^ cfg.region; "AWS_PAGER=" ] @ match cfg.profile with Some p -> [ "AWS_PROFILE=" ^ p ] | None -> [] in
  Array.of_list (keep @ extra)

let fields = String.split_on_char '\t'
let opt s = if s = "None" || s = "" then None else Some s
let ms s = Option.bind (opt s) (fun s -> Option.map (fun f -> f /. 1000.) (float_of_string_opt s))

(* name, id, startedAt per job of one status *)
let list_jobs cfg status =
  [|
    "aws"; "batch"; "list-jobs"; "--job-queue"; cfg.queue; "--job-status"; status;
    "--query"; Printf.sprintf "jobSummaryList[?starts_with(jobName,`%s`)].[jobName,jobId,startedAt]" cfg.prefix;
    "--output"; "text";
  |]

let describe_jobs ids =
  Array.of_list
    ([ "aws"; "batch"; "describe-jobs"; "--jobs" ] @ ids
    @ [ "--query"; "jobs[].[jobId,status,container.logStreamName,jobDefinition,startedAt,stoppedAt,statusReason]"; "--output"; "text" ])

(* exactly this stream, if it exists: a tail over a stream that does not exist yet fails as a whole *)
let stream_exists cfg stream =
  [| "aws"; "logs"; "describe-log-streams"; "--log-group-name"; cfg.group; "--log-stream-name-prefix"; stream; "--query"; "logStreams[].logStreamName"; "--output"; "text" |]

(* [aws logs tail --follow] block-buffers its output when stdout is a pipe
   and nothing arrives until it exits, so it runs under a pty from script(1). *)
let tail cfg streams =
  let argv = [ "aws"; "logs"; "tail"; cfg.group; "--log-stream-names" ] @ streams @ [ "--follow"; "--format"; "detailed"; "--color"; "off"; "--since"; cfg.since ] in
  [| "script"; "-qfec"; String.concat " " (List.map Filename.quote argv); "/dev/null" |]

let terminate_job id = [| "aws"; "batch"; "terminate-job"; "--job-id"; id; "--reason"; "killed from fargate tui" |]
let cancel_job id = [| "aws"; "batch"; "cancel-job"; "--job-id"; id; "--reason"; "killed from fargate tui" |]

(* ---- timestamps ---- *)

let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let doy = ((153 * if m > 2 then m - 3 else m + 9) + 2) / 5 + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

(* "2026-10-10T09:58:47.385000+00:00" -> epoch seconds *)
let epoch_of_iso s =
  try
    Scanf.sscanf s "%4d-%2d-%2dT%2d:%2d:%f%s" (fun y mo d h mi sec rest ->
        let base = (float_of_int (days_from_civil y mo d) *. 86400.) +. float_of_int ((h * 3600) + (mi * 60)) +. sec in
        let off =
          match rest with
          | "" | "Z" -> 0.
          | _ -> Scanf.sscanf rest "%c%2d:%2d" (fun sign oh om -> (if sign = '-' then -1. else 1.) *. float_of_int ((oh * 3600) + (om * 60)))
        in
        Some (base -. off))
  with _ -> None

let hms t =
  let tm = Unix.localtime t in
  Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec

let duration secs =
  let s = max 0 (int_of_float secs) in
  if s < 60 then Printf.sprintf "%ds" s
  else if s < 3600 then Printf.sprintf "%dm%02ds" (s / 60) (s mod 60)
  else Printf.sprintf "%dh%02dm" (s / 3600) (s mod 3600 / 60)

let progress_re = Str.regexp "[0-9,]+/[0-9,]+ *([0-9]+%[^)]*)"
let progress_of msg = match Str.search_forward progress_re msg 0 with _ -> Some (Str.matched_string msg) | exception Not_found -> None

(* "v11f-z30y2016" sorts by zone number then year *)
let zone_key name =
  match Scanf.sscanf name "%_[^z]z%dy%d%!" (fun z y -> (z, y)) with k -> k | exception _ -> (max_int, 0)
