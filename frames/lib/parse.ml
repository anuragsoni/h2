open Base
open Angstrom
open Types

let frame_length =
  lift3
    (fun x y z -> (x lsl 16) lor (y lsl 8) lor z)
    any_uint8 any_uint8 any_uint8

let frame_type = lift (fun x -> frame_type_to_id x) any_uint8

let frame_flags = any_uint8

let stream_identifier =
  lift (fun x -> Int32.to_int_exn x land ((1 lsl 31) - 1)) Angstrom.BE.any_int32

let parse_payload_with_padding frame_header parse_fn =
  if test_padded frame_header.flags then
    any_uint8
    >>= fun pad_length ->
    let body_lenth = frame_header.length - pad_length - 1 in
    if body_lenth < 0 then
      fail ("padding is not enough " ^ Int.to_string pad_length)
    else parse_fn body_lenth
  else parse_fn frame_header.length

let parse_data_frame frame_header =
  let parse_data length = lift (fun x -> DataFrame x) (take length) in
  parse_payload_with_padding frame_header parse_data

let parse_priority =
  lift2
    (fun s w ->
      let s' = Int32.to_int_exn s in
      let e = test_bit s' 32 in
      let p =
        {exclusive = e; weight = w; stream_dependency = s' land ((1 lsl 31) - 1)}
      in
      p )
    Angstrom.BE.any_int32 any_uint8

let parse_priority_frame = parse_priority >>| fun x -> PriorityFrame x

let parse_header_frame frame_header =
  let parse_fn =
    if test_priority frame_header.flags then fun length ->
      lift2
        (fun priority headers -> HeadersFrame (Some priority, headers))
        parse_priority
        (take (length - 5))
    else fun length -> lift (fun x -> HeadersFrame (None, x)) (take length)
  in
  parse_payload_with_padding frame_header parse_fn

let parse_error_code =
  lift (fun x -> error_code_to_id (Int32.to_int_exn x)) Angstrom.BE.any_int32

let parse_rst_stream = lift (fun x -> RSTStreamFrame x) parse_error_code

let parse_settings_frame frame_header =
  let num_settings = frame_header.length / 6 in
  let parse_setting =
    lift2
      (fun k v ->
        Option.map (settings_key_to_id k) ~f:(fun s -> (s, Int32.to_int_exn v))
        )
      BE.any_int16 BE.any_int32
  in
  lift
    (fun s -> SettingsFrame (List.filter_opt s))
    (* TODO: This ignores unknown settings, check if this needs to be a protocol
       error. *)
    (count num_settings parse_setting)

let parse_push_promise_frame frame_header =
  let parse_fn length =
    lift2
      (fun s h -> PushPromiseFrame (s, h))
      stream_identifier
      (take (length - 4))
  in
  parse_payload_with_padding frame_header parse_fn

let parse_ping_frame = lift (fun x -> PingFrame x) (take 8)

let parse_go_away frame_header =
  lift3
    (fun s e x -> GoAwayFrame (s, e, x))
    stream_identifier parse_error_code
    (take (frame_header.length - 8))

let parse_window_frame =
  BE.any_int32
  >>= fun w ->
  let w' = clear_bit (Int32.to_int_exn w) 31 in
  if w' = 0 then fail "Window update must not be 0"
  else return (WindowUpdateFrame w')

let parse_continuation_frame frame_header =
  lift (fun x -> ContinuationFrame x) (take frame_header.length)

let parse_unknown_frame typ frame_header =
  lift (fun x -> UnknownFrame (typ, x)) (take frame_header.length)

let get_parser_for_frame frame_header frame_type =
  match frame_type with
  | FrameData -> parse_data_frame frame_header
  | FrameHeaders -> parse_header_frame frame_header
  | FramePriority -> parse_priority_frame
  | FrameRSTStream -> parse_rst_stream
  | FrameSettings -> parse_settings_frame frame_header
  | FramePushPromise -> parse_push_promise_frame frame_header
  | FramePing -> parse_ping_frame
  | FrameGoAway -> parse_go_away frame_header
  | FrameWindowUpdate -> parse_window_frame
  | FrameContinuation -> parse_continuation_frame frame_header
  | FrameUnknown typ -> parse_unknown_frame typ frame_header

let parse_frame_header =
  lift4
    (fun length frame_type flags stream_id ->
      (frame_type, {flags; length; stream_id}) )
    frame_length frame_type frame_flags stream_identifier
  <* commit

let parse_frame_payload frame_type frame_header =
  get_parser_for_frame frame_header frame_type

let parse_frame =
  parse_frame_header
  >>= fun (frame_type, frame_header) ->
  get_parser_for_frame frame_header frame_type
  >>| fun frame_payload -> {frame_header; frame_payload}
