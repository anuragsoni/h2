(** Headers frame is used to open a stream, and additionally carries a
    header block fragment. HEADERS frames can be sent on a stream in the "idle", "reserved(local)", "open", or "half-closed (remote) state".
*)

type t =
  { stream_id: int
  ; stream_dependency: Priority_frame.stream_dependency option
  ; flags: int
  ; header_block_fragment: Bitstring.t}
[@@deriving fields]
