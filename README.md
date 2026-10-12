# fargate

A TUI for the `v11f-` zarr-fill jobs in the `tessera-zarr` AWS Batch queue.
It replaces the `running-v11.sh` / `stream-v11.sh` / `kill-v11.sh` loop with
one screen:

- **left pane**: every RUNNING or STARTING job, as `running-v11.sh` lists
  them, with job definition, uptime, seconds since its last log line, a
  state, and the progress from that line.
- **right pane**: the selected job's CloudWatch log, as `stream-v11.sh`
  follows it. Every job's stream is tailed all the time, so flicking between
  jobs with the arrow keys is instant and nothing is missed.
- **STALLED**: a running job that has written nothing to its log for more
  than 60 seconds (`--stall`). Shown in red, and counted in the header.
  **HUNG** is the old `0.0/s` test from the scripts.
- **K** (or Delete) terminates the selected job after a y/n confirmation,
  as `kill-v11.sh` does for a running job. Written shards are not touched.

Jobs that leave the queue stay listed, dimmed, for fifteen minutes with
their final status and reason.

```
fargate                 # the defaults match the v11 scripts
fargate --prefix v1f-   # another job family
fargate --stall 120     # be more patient
fargate --help
```

Options: `--queue`, `--prefix`, `--log-group`, `--region`, `--profile`,
`--stall`, `--since` (how much history a restarted tail reads), `--refresh`
(seconds between queue polls).

## Keys

| key | |
|---|---|
| ↑ ↓ / j k | select a job |
| PgUp PgDn | scroll the log; End returns to the tail |
| K / Delete | terminate the selected job (asks y/n) |
| r | poll the queue now |
| q | quit |

## How it works

`aws batch list-jobs` is polled every 15 seconds for RUNNING, STARTING and
RUNNABLE jobs. New jobs are described once to learn their log stream and job
definition. All known streams go into a single `aws logs tail --follow`
process, restarted when the set changes (at most every five seconds); events
are deduplicated by timestamp so the restart overlap is harmless. The tail
runs under `script(1)` because the aws CLI buffers its output when stdout is
a pipe. Each stream is confirmed to exist with `describe-log-streams` before
it is added, because one missing stream name makes the whole tail fail.

Stall time is measured from the CloudWatch event timestamp of the last log
line, not from when the line arrived here, so the figure is right from the
first screen even though a fresh tail replays the last `--since` of history.

## Building

notty needs OCaml < 5.4 (`.day10` pins 5.3):

```
day10 build .
_build/install/default/bin/fargate
```
