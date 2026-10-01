type t =
  | Int of int
  | String of string
  | List of t list
  | Dict of (string * t) list

let rec encode buf = function
  | Int n ->
      Buffer.add_char buf 'i';
      Buffer.add_string buf (string_of_int n);
      Buffer.add_char buf 'e'
  | String s ->
      Buffer.add_string buf (string_of_int (String.length s));
      Buffer.add_char buf ':';
      Buffer.add_string buf s
  | List l ->
      Buffer.add_char buf 'l';
      List.iter (encode buf) l;
      Buffer.add_char buf 'e'
  | Dict d ->
      Buffer.add_char buf 'd';
      List.iter
        (fun (k, v) ->
           encode buf (String k);
           encode buf v)
        (List.sort (fun (a, _) (b, _) -> String.compare a b) d);
      Buffer.add_char buf 'e'

(* Both limits are far above anything a real nREPL server produces;
   they only bound what a malformed stream can make us allocate or
   recurse. *)
(* Where [read] gets its bytes from. *)
type source = { next : unit -> char; take : int -> string }

let max_string_length = 64 * 1024 * 1024
let max_depth = 64

let read_digits src stop =
  let buf = Buffer.create 16 in
  let rec loop () =
    if Buffer.length buf > 20 then
      failwith "Bencode.read: integer too long";
    let c = src.next () in
    if c = stop then
      match int_of_string_opt (Buffer.contents buf) with
        | Some n -> n
        | None -> failwith "Bencode.read: invalid integer"
    else begin
      Buffer.add_char buf c;
      loop ()
    end
  in
  loop ()

let read_string src first =
  let buf = Buffer.create 16 in
  Buffer.add_char buf first;
  let rec loop () =
    if Buffer.length buf > 20 then
      failwith "Bencode.read: string length too long";
    let c = src.next () in
    if c = ':' then
      match int_of_string_opt (Buffer.contents buf) with
        | Some n when n >= 0 && n <= max_string_length ->
            src.take n
        | _ -> failwith "Bencode.read: invalid string length"
    else begin
      Buffer.add_char buf c;
      loop ()
    end
  in
  loop ()

let rec read_value src depth first =
  if depth > max_depth then
    failwith "Bencode.read: value nested too deeply";
  match first with
    | 'i' ->
        Int (read_digits src 'e')
    | 'l' ->
        let rec loop acc =
          let c = src.next () in
          if c = 'e' then
            List (List.rev acc)
          else
            loop (read_value src (depth + 1) c :: acc)
        in
        loop []
    | 'd' ->
        let rec loop acc =
          let c = src.next () in
          if c = 'e' then
            Dict (List.rev acc)
          else
            let key =
              match read_value src (depth + 1) c with
                | String s -> s
                | _ -> failwith "Bencode.read: dictionary key is not a string"
            in
            let value = read_value src (depth + 1) (src.next ()) in
            loop ((key, value) :: acc)
        in
        loop []
    | '0' .. '9' ->
        String (read_string src first)
    | c ->
        Printf.ksprintf failwith "Bencode.read: unexpected character %C" c

let read_from ~next ~take =
  let src = { next; take } in
  read_value src 0 (next ())

let read ic =
  read_from ~next:(fun () -> input_char ic) ~take:(really_input_string ic)

let get v key =
  match v with
    | Dict d -> List.assoc_opt key d
    | _ -> None

let get_string v key =
  match get v key with
    | Some (String s) -> Some s
    | _ -> None

let get_strings v key =
  match get v key with
    | Some (List l) ->
        List.filter_map (function String s -> Some s | _ -> None) l
    | _ -> []
