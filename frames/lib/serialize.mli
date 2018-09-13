type frame_info =
  { flags : Types.frame_flags
  ; stream_id : Types.stream_id
  ; padding : Types.padding option }

val make_frame_info : set_flag:(int -> int) -> stream_id:int32 -> frame_info

val write_frame : Faraday.t -> frame_info -> Types.frame_payload -> unit
