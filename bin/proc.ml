(* Child processes with their output on a non-blocking pipe. Each child runs
   in its own session so that killing the process group takes down anything
   it spawned (the aws CLI is a Python program with helpers). *)

type t = {
  pid : int;
  fd : Unix.file_descr;
  partial : Buffer.t;
  mutable open_ : bool;  (* not yet at EOF *)
  mutable closed : bool;
}

let spawn ?(env = Unix.environment ()) argv =
  let r, w = Unix.pipe ~cloexec:true () in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
  match Unix.fork () with
  | 0 -> (
      try
        ignore (Unix.setsid ());
        Sys.set_signal Sys.sigpipe Sys.Signal_default;
        Unix.dup2 devnull Unix.stdin;
        Unix.dup2 w Unix.stdout;
        Unix.dup2 w Unix.stderr;
        Unix.execvpe argv.(0) argv env
      with _ -> Unix._exit 127)
  | pid ->
      Unix.close w;
      Unix.close devnull;
      Unix.set_nonblock r;
      { pid; fd = r; partial = Buffer.create 256; open_ = true; closed = false }

(* Drop escape sequences (CSI and two-byte ones); [keep] decides about the other characters. *)
let strip_escapes ?(keep = Buffer.add_char) line =
  let b = Buffer.create (String.length line) in
  let n = String.length line in
  let rec go i =
    if i >= n then ()
    else if line.[i] = '\027' then begin
      if i + 1 < n && line.[i + 1] = '[' then begin
        let j = ref (i + 2) in
        while !j < n && not (line.[!j] >= '@' && line.[!j] <= '~') do
          incr j
        done;
        go (!j + 1)
      end
      else go (i + 2)
    end
    else begin
      keep b line.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents b

(* Terminal-ish cleanup: drop escape sequences, keep the text after the last
   carriage return (progress bars), and make it printable. *)
let clean line =
  let line =
    let n = String.length line in
    if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line
  in
  let line = match String.rindex_opt line '\r' with Some i -> String.sub line (i + 1) (String.length line - i - 1) | None -> line in
  let s =
    strip_escapes
      ~keep:(fun b c -> match c with '\t' -> Buffer.add_string b "  " | c when Char.code c < 32 || Char.code c = 127 -> () | c -> Buffer.add_char b c)
      line
  in
  if String.is_valid_utf_8 s then s else String.map (fun c -> if Char.code c >= 128 then '?' else c) s

let chunk = Bytes.create 65536

(* Read whatever is available, calling [f] on each complete raw line. *)
let read t f =
  let rec loop () =
    match Unix.read t.fd chunk 0 (Bytes.length chunk) with
    | 0 ->
        t.open_ <- false;
        if Buffer.length t.partial > 0 then begin
          f (Buffer.contents t.partial);
          Buffer.clear t.partial
        end
    | n ->
        let s = Bytes.sub_string chunk 0 n in
        let rec emit = function
          | [] -> ()
          | [ last ] -> Buffer.add_string t.partial last
          | l :: rest ->
              Buffer.add_string t.partial l;
              f (Buffer.contents t.partial);
              Buffer.clear t.partial;
              emit rest
        in
        emit (String.split_on_char '\n' s);
        loop ()
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) -> ()
    | exception Unix.Unix_error _ -> t.open_ <- false
  in
  if t.open_ && not t.closed then loop ()

let close t =
  t.open_ <- false;
  if not t.closed then begin
    t.closed <- true;
    try Unix.close t.fd with Unix.Unix_error _ -> ()
  end

(* Some exit code once the process has finished. *)
let poll t =
  match Unix.waitpid [ Unix.WNOHANG ] t.pid with
  | 0, _ -> None
  | _, Unix.WEXITED n -> Some n
  | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> Some (128 + abs n)
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> Some 255

let kill ?(signal = Sys.sigterm) t = try Unix.kill (-t.pid) signal with Unix.Unix_error _ -> ()
