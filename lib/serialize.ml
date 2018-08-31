open Faraday

type frame_info =
  { flags : Types.frame_flags
  ; stream_id : Types.stream_id
  ; padding : Types.padding option }

let make_frame_info set_flag stream_id =
  {flags = set_flag Types.default_flags; stream_id; padding = None}

let write_24 t i =
  let w0 = (i lsr 16) land 0xff in
  let w1 = (i lsr 8) land 0xff in
  let w2 = i land 0xff in
  write_uint8 t w0 ; write_uint8 t w1 ; write_uint8 t w2

let write_16 t i =
  let w0 = (i lsr 8) land 0xff in
  let w1 = i land 0xff in
  write_uint8 t w0 ; write_uint8 t w1

let write_frame_header t frame_type {Types.length; flags; stream_id} =
  write_24 t length ;
  write_uint8 t (Types.frame_type_of_id frame_type) ;
  write_uint8 t flags ;
  BE.write_uint32 t (Int32.of_int stream_id)

let write_padded info length writer =
  match info.padding with
  | None ->
      let header =
        {Types.length; flags = info.flags; stream_id = info.stream_id}
      in
      (header, writer)
  | Some padding ->
      let flags = Types.set_padded info.flags in
      let pad_length = String.length padding in
      let new_length = length + pad_length + 1 in
      let new_writer t =
        write_uint8 t pad_length ; writer t ; write_string t padding
      in
      let header =
        {Types.length = new_length; flags; stream_id = info.stream_id}
      in
      (header, new_writer)

let write_data_frame info body =
  let writer t = write_string t body in
  let length = String.length body in
  write_padded info length writer

let get_writer info frame =
  let open Types in
  match frame with
  | DataFrame body -> write_data_frame info body
  | _ -> failwith "Not implemented yet"

let write_frame t info payload =
  let header, writer = get_writer info payload in
  let ft = Types.frame_payload_to_frame_id payload in
  write_frame_header t ft header ;
  writer t
