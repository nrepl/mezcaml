open Mezcaml

type stream = {
  channel : out_channel;
  tty : bool;
  mutable at_line_start : bool;
}

(* Where the terminal's cursor is, as far as mezcaml can tell: at the
   start of a line, after something one of the streams going to it
   printed without a final newline, or after the ^C it echoed. *)
type terminal = Line_start | Mid_line of stream | After_ctrl_c

(* What came in while linenoise had the terminal, to print once it
   lets go. *)
type held = Stray of Bencode.t | Report of string

type t = {
  out : stream;
  err : stream;
  out_color : bool;
  err_color : bool;
  held : held Queue.t;
  mutable holding : bool;
  mutable terminal : terminal;
}

let stream channel tty =
  let tty =
    match tty with
      | Some tty -> tty
      | None -> Unix.isatty (Unix.descr_of_out_channel channel)
  in
  { channel; tty; at_line_start = true }

let create ?(out = stdout) ?(err = stderr) ?out_tty ?err_tty ~out_color
    ~err_color () =
  { out = stream out out_tty;
    err = stream err err_tty;
    out_color;
    err_color;
    held = Queue.create ();
    holding = false;
    terminal = Line_start }

let style enabled code text =
  if enabled then Printf.sprintf "\027[%sm%s\027[0m" code text else text

(* mezcaml ignores SIGPIPE, so that a server going away is an error it
   can report. A stdout going away (mezcaml -e ... | head) still ends
   it the way SIGPIPE always did, and any other trouble writing it
   (a full disk) is an error too. A stderr in trouble is let go: there
   would be nowhere to say so. *)
let guarded t stream f =
  try f () with
    | Sys_error message when stream == t.out ->
        if message = Unix.error_message Unix.EPIPE then begin
          Sys.set_signal Sys.sigpipe Sys.Signal_default;
          Unix.kill (Unix.getpid ()) Sys.sigpipe
        end
        else begin
          (try prerr_endline ("cannot write the output: " ^ message)
           with Sys_error _ -> ());
          exit 1
        end
    | Sys_error _ -> ()

let newline t stream =
  guarded t stream (fun () -> output_char stream.channel '\n');
  stream.at_line_start <- true;
  if stream.tty then t.terminal <- Line_start

let write t stream ?(color = "") text =
  if text <> "" then begin
    (match t.terminal with
       | After_ctrl_c when stream.tty -> newline t stream
       | _ -> ());
    let colored =
      color <> "" && (if stream == t.out then t.out_color else t.err_color)
    in
    guarded t stream (fun () ->
      output_string stream.channel (style colored color text));
    let ends_line = text.[String.length text - 1] = '\n' in
    stream.at_line_start <- ends_line;
    if stream.tty then
      t.terminal <- (if ends_line then Line_start else Mid_line stream)
  end

let flush_all t =
  guarded t t.out (fun () -> flush t.out.channel);
  guarded t t.err (fun () -> flush t.err.channel)

let fresh_line t =
  (match t.terminal with
     | Line_start -> ()
     | Mid_line stream -> newline t stream
     | After_ctrl_c -> newline t (if t.err.tty then t.err else t.out));
  flush_all t

let end_line t =
  newline t t.out;
  flush_all t

let ctrl_c_echoed t =
  if t.out.tty || t.err.tty then t.terminal <- After_ctrl_c

let report t text =
  if t.holding then Queue.push (Report text) t.held
  else begin
    if t.err.tty then fresh_line t
    else if not t.err.at_line_start then newline t t.err;
    write t t.err ~color:"31" (text ^ "\n");
    flush_all t
  end

(* Some servers (utop's multi-phrase evals, for one) send values with
   leading newlines; they only add stray blank lines on a terminal. *)
let strip_leading_newlines s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && s.[!i] = '\n' do incr i done;
  if !i = 0 then s else String.sub s !i (n - !i)

let print_output t msg =
  (match Bencode.get_string msg "out" with
     | Some out -> write t t.out out
     | None -> ());
  (match Bencode.get_string msg "err" with
     | Some err -> write t t.err ~color:"31" err
     | None -> ());
  flush_all t

let print_response t msg =
  print_output t msg;
  (match Bencode.get_string msg "value" with
     | Some value ->
         let value = strip_leading_newlines value in
         write t t.out ~color:"32" value;
         if not t.out.at_line_start || value = "" then newline t t.out
     | None -> ());
  flush_all t

let stray t msg =
  if t.holding then Queue.push (Stray msg) t.held else print_output t msg

let hold t f =
  t.holding <- true;
  Fun.protect f ~finally:(fun () -> t.holding <- false)

let release t =
  let held = Queue.copy t.held in
  Queue.clear t.held;
  Queue.iter
    (function Stray msg -> print_output t msg | Report text -> report t text)
    held
