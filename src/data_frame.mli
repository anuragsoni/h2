(** Data frames convey arbitrary, variable-length sequences of octets
    associated with a stream. One or more DATA frames are used, for instance,
    to carry HTTP request or response payloads. *)

type t = {data: string; stream_id: int; flags: int; padding_length: int option}
[@@deriving fields]
