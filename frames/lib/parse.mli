open Types

val parse_frame_header : (frame_type_id * frame_header) Angstrom.t

val parse_frame : frame Angstrom.t

val parse_frame_payload :
  frame_type_id -> frame_header -> frame_payload Angstrom.t
