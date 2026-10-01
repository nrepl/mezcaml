(** Printing what the server sends the way a terminal user expects:
    evaluation output as it was written, errors in red on stderr,
    values in green on their own line. *)

type t

val create :
  ?out:out_channel -> ?err:out_channel -> out_color:bool -> err_color:bool ->
  unit -> t
(** Output to [out] and [err] (stdout and stderr by default), colored
    where asked for. *)

val print_response : t -> Mezcaml.Bencode.t -> unit
(** A response to the request being read: its out, err and value. *)

val print_output : t -> Mezcaml.Bencode.t -> unit
(** Just the out and err of a response. *)

val stray : t -> Mezcaml.Bencode.t -> unit
(** A reply that belongs to no request still waiting for it (a future
    printing after its eval finished): its output gets printed, unless
    it's being held (see {!hold}). *)

val hold : t -> (unit -> 'a) -> 'a
(** [hold t f] runs [f], holding back any strays meanwhile. That's for
    requests made while linenoise has the terminal in raw mode (Tab
    completion), where printing would garble the line being edited. *)

val release : t -> unit
(** Print the strays held back so far. *)
