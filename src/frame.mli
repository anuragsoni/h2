(** Frames are the building blocks of HTTP/2 protocol.

    All frames begin with a fixed 9-octet header followed by
    a variable-length payload.

    {[
    +-----------------------------------------------+
    |                 Length (24)                   |
    +---------------+---------------+---------------+
    |   Type (8)    |   Flags (8)   |
    +-+-------------+---------------+-------------------------------+
    |R|                 Stream Identifier (31)                      |
    +=+=============================================================+
    |                   Frame Payload (0...)                      ...
    +---------------------------------------------------------------+
    ]}
*)

type t =
  | Data
  | Headers
  | Priority
  | RSTStream
  | Settings
  | PushPromise
  | Ping
  | GoAway
  | WindowUpdate
  | Continuation

val t_to_id : t -> int
(** [frame_type_to_id] converts a [frame_type] to its integer representation *)

val t_of_id : int -> (t, string) result
(** [frame_type_of_id] converts an integer to frame_type *)
