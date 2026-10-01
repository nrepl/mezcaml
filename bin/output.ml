open Mezcaml

type t = {
  out : out_channel;
  err : out_channel;
  out_color : bool;
  err_color : bool;
  held : Bencode.t Queue.t;
  mutable holding : bool;
}

let create ?(out = stdout) ?(err = stderr) ~out_color ~err_color () =
  { out; err; out_color; err_color; held = Queue.create (); holding = false }

let style enabled code text =
  if enabled then Printf.sprintf "\027[%sm%s\027[0m" code text else text

(* Some servers (utop's multi-phrase evals, for one) send values with
   leading newlines; they only add stray blank lines on a terminal. *)
let strip_leading_newlines s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && s.[!i] = '\n' do incr i done;
  if !i = 0 then s else String.sub s !i (n - !i)

let print_output t msg =
  (match Bencode.get_string msg "out" with
     | Some out -> output_string t.out out
     | None -> ());
  (match Bencode.get_string msg "err" with
     | Some err -> output_string t.err (style t.err_color "31" err)
     | None -> ());
  flush t.out;
  flush t.err

let print_response t msg =
  print_output t msg;
  (match Bencode.get_string msg "value" with
     | Some value ->
         let value = strip_leading_newlines value in
         output_string t.out (style t.out_color "32" value);
         if value = "" || value.[String.length value - 1] <> '\n' then
           output_char t.out '\n'
     | None -> ());
  flush t.out

let stray t msg =
  if t.holding then Queue.push msg t.held else print_output t msg

let hold t f =
  t.holding <- true;
  Fun.protect f ~finally:(fun () -> t.holding <- false)

let release t =
  let held = Queue.copy t.held in
  Queue.clear t.held;
  Queue.iter (print_output t) held
