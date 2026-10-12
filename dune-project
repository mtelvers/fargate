(lang dune 3.19)

(name fargate)

(generate_opam_files true)

(authors "Mark Elvers <mark.elvers@tunbury.org>")

(maintainers "Mark Elvers <mark.elvers@tunbury.org>")

(license MIT)

(package
 (name fargate)
 (synopsis "TUI for AWS Batch fill jobs: running list, live logs, stall detection, kill")
 (description "fargate watches the v11f- jobs in an AWS Batch queue. The left pane lists the running jobs with their job definition, uptime, time since last log output and progress; the right pane streams the selected job's CloudWatch log. Every log stream is tailed continuously so switching between jobs is instant. A job that has produced no output for more than a configurable number of seconds is flagged STALLED, and a key terminates the selected job.")
 (depends
  (ocaml (>= 5.1))
  notty
  cmdliner))
