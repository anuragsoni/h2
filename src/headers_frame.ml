type t =
  { stream_id: int
  ; stream_dependency: Priority_frame.stream_dependency option
  ; flags: int
  ; header_block_fragment: Bitstring.t
  (* TODO: this is a placeholder till we implement header_block_fragment *) }
[@@deriving fields]
