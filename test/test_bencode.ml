open Mezcaml.Bencode

let read_back encoded =
  let file = Filename.temp_file "mezcaml-test" ".bencode" in
  let oc = open_out_bin file in
  output_string oc encoded;
  close_out oc;
  let ic = open_in_bin file in
  let value = read ic in
  close_in ic;
  Sys.remove file;
  value

let encode_to_string value =
  let buf = Buffer.create 64 in
  encode buf value;
  Buffer.contents buf

let roundtrip value =
  assert (read_back (encode_to_string value) = value)

let fails encoded =
  match read_back encoded with
    | exception Failure _ -> ()
    | _ -> assert false

let () =
  (* Round trips. *)
  roundtrip (Int 42);
  roundtrip (Int (-7));
  roundtrip (String "");
  roundtrip (String "hello");
  roundtrip (String "embedded\x00bytes\xff");
  roundtrip (List []);
  roundtrip (List [Int 1; String "two"; List [Int 3]]);
  (* Keys already sorted: encode sorts them, so only sorted dicts
     round-trip to a structurally equal value. *)
  roundtrip (Dict []);
  roundtrip (Dict [("code", String "1 + 2"); ("op", String "eval")]);
  roundtrip (Dict [("a", List [Dict [("b", Int 1)]])]);

  (* Dictionary keys are sorted on encode. *)
  assert (encode_to_string (Dict [("b", Int 2); ("a", Int 1)])
          = encode_to_string (Dict [("a", Int 1); ("b", Int 2)]));

  (* Wire format spot checks. *)
  assert (encode_to_string (Int 42) = "i42e");
  assert (encode_to_string (String "spam") = "4:spam");
  assert (encode_to_string (List [Int 1]) = "li1ee");

  (* Malformed input is rejected rather than crashing. *)
  fails "x";
  fails "ixe";
  fails "9999999999999999999999999999:";
  fails "4611686018427387903:";
  fails (String.make 100 'l');

  print_endline "all bencode tests passed"

(* The truncated-dict case raises End_of_file, not Failure: check it
   separately so [fails] can stay strict. *)
let () =
  match read_back "d3:key" with
    | exception End_of_file -> ()
    | exception Failure _ -> ()
    | _ -> assert false
