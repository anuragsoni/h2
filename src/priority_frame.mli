(** Priority frame specifies the sender-advised priority of a stream.
    It can be sent in any stream state, including idle or closed streams. *)

type stream_dependency = {dependency_id: int; weight: int; is_exclusive: bool}
[@@deriving fields]

type t = {stream_id: int; dependency: stream_dependency} [@@deriving fields]
