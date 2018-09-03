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

let write_priority {Types.exclusive; stream_dependency; weight} =
  let stream =
    if exclusive then Types.set_exclusive stream_dependency
    else stream_dependency
  in
  fun t ->
    BE.write_uint32 t (Int32.of_int stream) ;
    write_uint8 t weight

let write_priority_frame info priority =
  let header =
    {Types.flags = info.flags; stream_id = info.stream_id; length = 5}
  in
  (header, write_priority priority)

let write_rst_stream_frame info e =
  let error_code = Types.error_code_of_id e in
  let header =
    {Types.flags = info.flags; stream_id = info.stream_id; length = 4}
  in
  (header, fun t -> BE.write_uint32 t (Int32.of_int error_code))

let write_settings_frame info settings =
  let writer t =
    let rec aux = function
      | [] -> ()
      | (key, value) :: xs ->
          BE.write_uint16 t (Types.settings_key_from_id key) ;
          BE.write_uint32 t (Int32.of_int value) ;
          aux xs
    in
    aux settings
  in
  let header =
    { Types.flags = info.flags
    ; stream_id = info.stream_id
    ; length = List.length settings * 6 }
  in
  (header, writer)

let write_push_promise_frame info stream header_block =
  let length = 4 + String.length header_block in
  let writer t =
    BE.write_uint32 t (Int32.of_int stream) ;
    write_string t header_block
  in
  write_padded info length writer

let write_ping_frame info payload =
  let header =
    {Types.flags = info.flags; stream_id = info.stream_id; length = 8}
  in
  let writer t = write_string t payload in
  (header, writer)

let write_go_away_frame info stream_id error_code_id debug_data =
  let header =
    { Types.flags = info.flags
    ; stream_id = info.stream_id
    ; length = 8 + String.length debug_data }
  in
  let writer t =
    BE.write_uint32 t (Int32.of_int stream_id) ;
    BE.write_uint32 t (Int32.of_int (Types.error_code_of_id error_code_id)) ;
    write_string t debug_data
  in
  (header, writer)

let write_window_frame info window_size =
  let header =
    {Types.flags = info.flags; stream_id = info.stream_id; length = 4}
  in
  (* TODO: How to handle reserved bit? *)
  let writer t = BE.write_uint32 t (Int32.of_int window_size) in
  (header, writer)

let get_writer info frame =
  match frame with
  | Types.DataFrame body -> write_data_frame info body
  | Types.PriorityFrame p -> write_priority_frame info p
  | Types.RSTStreamFrame e -> write_rst_stream_frame info e
  | Types.SettingsFrame settings -> write_settings_frame info settings
  | Types.PushPromiseFrame (stream, header_block) ->
      write_push_promise_frame info stream header_block
  | Types.PingFrame payload -> write_ping_frame info payload
  | Types.GoAwayFrame (stream_id, error_code_id, debug_data) ->
      write_go_away_frame info stream_id error_code_id debug_data
  | Types.WindowUpdateFrame window_size -> write_window_frame info window_size
  | _ -> failwith "Not implemented yet"

let write_frame t info payload =
  let header, writer = get_writer info payload in
  let ft = Types.frame_payload_to_frame_id payload in
  write_frame_header t ft header ;
  writer t
