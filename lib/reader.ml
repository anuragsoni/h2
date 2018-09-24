open Base
module FT = Types

type continuable = [`Headers | `PushPromise]

type partial = {frame : continuable; buffer : Bigstringaf.t}

type t = {partial : partial option}

let create = {partial = None}

let frame_header = Angstrom.parse_bigstring Parse.parse_frame_header

let frame_payload frame_type frame_header =
  Angstrom.parse_bigstring (Parse.parse_frame_payload frame_type frame_header)

let read_frame conn bs =
  let open Result.Monad_infix in
  let create_error m = FT.ConnectionError (FT.ProtocolError, m) in
  Result.map_error ~f:create_error (frame_header bs)
  >>= fun (frame_type, frame_header) ->
  Frame_header.check_frame_header conn frame_header frame_type
  >>= fun frame_header ->
  Result.map_error ~f:create_error
    (frame_payload frame_type frame_header
       (Bigstringaf.sub bs ~off:9 ~len:frame_header.length))
