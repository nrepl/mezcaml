(** Bencode codec for the default nREPL transport.

    Only the subset the protocol needs is supported: integers, byte
    strings, lists and dictionaries. *)

type t =
  | Int of int
  | String of string
  | List of t list
  | Dict of (string * t) list

val encode : Buffer.t -> t -> unit
(** [encode buf v] appends the bencode representation of [v] to
    [buf]. Dictionary keys are emitted in sorted order, as the format
    requires. *)

val read : in_channel -> t
(** [read ic] reads one bencode value from [ic]. Raises [End_of_file]
    if the channel is exhausted and [Failure] on malformed input. *)

(** {2 Accessors}

    Convenience helpers for picking fields out of response
    dictionaries. They return [None]/[[]] when the key is missing or
    the value has an unexpected shape. *)

val get : t -> string -> t option
val get_string : t -> string -> string option
val get_strings : t -> string -> string list
