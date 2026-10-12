open Notty_unix

let stop_requested = ref false

let run_tui (m : Model.t) =
  let ui = Tui.create () in
  let term = Term.create () in
  let tty = fst (Term.fds term) in
  let quit = ref false in
  let last_draw = ref 0. in
  (* whatever happens to the terminal, the aws processes must not outlive us *)
  Fun.protect ~finally:(fun () -> Model.shutdown m) @@ fun () ->
  while not !quit do
    Model.pump m;
    let now = Unix.gettimeofday () in
    if now -. !last_draw > 0.1 then begin
      Term.image term (Tui.render m ui (Term.size term));
      last_draw := now
    end;
    let readable = try (fun (r, _, _) -> r) (Unix.select (tty :: Model.fds m) [] [] 0.25) with Unix.Unix_error (Unix.EINTR, _, _) -> [] in
    if List.mem tty readable then begin
      let rec events () =
        (match Term.event term with
        | `Key key ->
            if Tui.handle m ui key then quit := true;
            last_draw := 0.
        | `Resize _ -> last_draw := 0.
        | `End -> quit := true
        | _ -> ());
        if (not !quit) && Term.pending term then events ()
      in
      events ()
    end;
    if !stop_requested then quit := true
  done;
  try Term.release term with Unix.Unix_error _ -> () (* the terminal may already be gone *)

let main region profile queue group prefix stall since refresh =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  List.iter (fun s -> Sys.set_signal s (Sys.Signal_handle (fun _ -> stop_requested := true))) [ Sys.sigterm; Sys.sighup; Sys.sigint ];
  let cfg = { Aws.region; profile; queue; group; prefix; stall; since; refresh } in
  run_tui (Model.create cfg);
  0

open Cmdliner

let region = Arg.(value & opt string "us-west-2" & info [ "region" ] ~docv:"REGION" ~doc:"AWS region.")
let profile = Arg.(value & opt (some string) (Some "default") & info [ "profile" ] ~docv:"PROFILE" ~doc:"AWS CLI profile.")
let queue = Arg.(value & opt string "tessera-zarr" & info [ "queue" ] ~docv:"QUEUE" ~doc:"AWS Batch job queue.")
let group = Arg.(value & opt string "/aws/batch/npy2zarr" & info [ "log-group" ] ~docv:"GROUP" ~doc:"CloudWatch log group the jobs write to.")
let prefix = Arg.(value & opt string "v11f-" & info [ "prefix" ] ~docv:"PREFIX" ~doc:"Only jobs whose name starts with $(docv).")
let stall = Arg.(value & opt float 60. & info [ "stall" ] ~docv:"SECONDS" ~doc:"A running job with no log output for longer than this is flagged STALLED.")
let since = Arg.(value & opt string "15m" & info [ "since" ] ~docv:"DURATION" ~doc:"How much log history a (re)started tail reads, as aws logs tail --since.")
let refresh = Arg.(value & opt float 15. & info [ "refresh" ] ~docv:"SECONDS" ~doc:"Seconds between polls of the job queue.")

let cmd =
  let doc = "watch AWS Batch fill jobs: running list, live logs, stall detection, kill" in
  let man =
    [
      `S Manpage.s_description;
      `P "The left pane lists the jobs in $(b,--queue) whose name starts with $(b,--prefix) that are RUNNING or STARTING, with their job definition, uptime, time since their last log line, and the progress reported by that line. The right pane shows the selected job's log. All log streams are tailed continuously, so switching between jobs is instant.";
      `P "A running job that has written nothing for more than $(b,--stall) seconds is shown as STALLED. A job whose last progress line reports 0.0/s is shown as HUNG. Jobs that leave the queue stay listed, dimmed, for fifteen minutes with their final status.";
      `S "KEYS";
      `P "Up/Down or j/k select a job; PgUp/PgDn scroll its log and End returns to the tail; K or Delete terminates the selected job after a y/n confirmation; r polls the queue now; q quits.";
    ]
  in
  Cmd.v (Cmd.info "fargate" ~doc ~man) Term.(const main $ region $ profile $ queue $ group $ prefix $ stall $ since $ refresh)

let () = exit (Cmd.eval' cmd)
