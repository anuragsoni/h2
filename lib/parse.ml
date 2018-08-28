open Base
open Angstrom
open Types

let zero_frame_types = [FrameSettings; FramePing; FrameGoAway]

let non_zero_frame_types =
  [ FrameData
  ; FrameHeaders
  ; FramePriority
  ; FrameRSTStream
  ; FramePushPromise
  ; FrameContinuation ]

(* TODO: Go back and verify all these checks against spec. *)
let check_frame_header settings frame_header =
  let open Polymorphic_compare in
  let {enable_push; _} = settings in
  let {flags; length; frame_type; stream_id} = frame_header in
  let check_frame_type = function
    | FrameData when stream_id = 0x0 ->
        Error
          (ConnectionError
             (ProtocolError, "data frames must be associated with a stream"))
    | FrameHeaders when test_padded flags && length < 1 ->
        Error
          (ConnectionError
             (FrameSizeError, "insufficient payload for padding length"))
    | FrameHeaders when test_priority flags && length < 5 ->
        Error
          (ConnectionError
             (FrameSizeError, "insufficient payload for priority fields"))
    | FrameHeaders when test_padded flags && test_priority flags && length < 6 ->
        Error
          (ConnectionError
             ( FrameSizeError
             , "insufficient payload for Pad length and priority fields" ))
    | FramePriority when not (length = 5) ->
        Error (StreamError (FrameSizeError, stream_id))
    | FrameRSTStream when not (length = 4) ->
        Error
          (ConnectionError
             (FrameSizeError, "payload length is not 4 in rst stream frame"))
    | FrameSettings when not (length % 6 = 0) ->
        Error
          (ConnectionError
             ( FrameSizeError
             , "payload length is not multiple of 6 in settings frame" ))
    | FrameSettings when test_ack flags && not (length = 0) ->
        Error
          (ConnectionError
             (FrameSizeError, "payload length must be 0 if ack flag is set"))
    | FramePushPromise when not enable_push ->
        Error (ConnectionError (ProtocolError, "push not enabled"))
    | FramePushPromise when not (is_response stream_id) ->
        Error
          (ConnectionError
             (ProtocolError, "push promise must be used with response streams"))
    | FramePing when not (length = 8) ->
        Error
          (ConnectionError (FrameSizeError, "payload length is 8 in ping frame"))
    | FrameGoAway when length < 8 ->
        Error
          (ConnectionError
             (FrameSizeError, "go away body must be 8 bytes or more"))
    | FrameWindowUpdate when not (length = 4) ->
        Error
          (ConnectionError
             (FrameSizeError, "payload length is 4 in window update frame"))
    | _ -> Ok {flags; length; frame_type; stream_id}
  in
  if length > settings.max_frame_size then
    Error (ConnectionError (FrameSizeError, "exceeded maximum frame size"))
  else if
    List.exists ~f:(fun x -> x = frame_type) non_zero_frame_types
    && is_control stream_id
  then Error (ConnectionError (ProtocolError, "cannot use in control stream"))
  else if
    List.exists ~f:(fun x -> x = frame_type) zero_frame_types
    && not (is_control stream_id)
  then Error (ConnectionError (ProtocolError, "cannot use in non-zero stream"))
  else check_frame_type frame_type

let frame_length =
  lift3
    (fun x y z -> (x lsl 16) lor (y lsl 8) lor z)
    any_uint8 any_uint8 any_uint8

let frame_type = lift (fun x -> frame_type_to_id x) any_uint8

let frame_flags = any_uint8

let stream_identifier =
  lift (fun x -> Int32.to_int_exn x land ((1 lsl 31) - 1)) Angstrom.BE.any_int32

let parse_frame_header =
  lift4
    (fun length frame_type flags stream_id ->
      {flags; length; frame_type; stream_id} )
    frame_length frame_type frame_flags stream_identifier

let parse_payload_with_padding frame_header parse_fn =
  if test_padded frame_header.flags then
    any_int8
    >>= fun pad_length ->
    let body_lenth = frame_header.length - pad_length - 1 in
    if body_lenth < 0 then
      return (Error (ConnectionError (ProtocolError, "padding is not enough")))
    else parse_fn body_lenth
  else parse_fn frame_header.length

let parse_data_frame frame_header =
  let parse_data length = lift (fun x -> Ok (DataFrame x)) (take length) in
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
    Angstrom.BE.any_int32 any_int8

let parse_priority_frame = parse_priority >>| fun x -> Ok (PriorityFrame x)

let parse_header_frame frame_header =
  let parse_fn =
    if test_priority frame_header.flags then fun length ->
      lift2
        (fun priority headers -> Ok (HeadersFrame (Some priority, headers)))
        parse_priority
        (take (length - 5))
    else fun length -> lift (fun x -> Ok (HeadersFrame (None, x))) (take length)
  in
  parse_payload_with_padding frame_header parse_fn

let parse_error_code =
  lift (fun x -> error_code_to_id (Int32.to_int_exn x)) Angstrom.BE.any_int32

let parse_rst_stream = lift (fun x -> Ok (RSTStreamFrame x)) parse_error_code

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
    (fun s -> Ok (SettingsFrame (List.filter_opt s)))
    (* TODO: This ignores unknown settings, check if this needs to be a protocol
       error. *)
    (count num_settings parse_setting)

let parse_push_promise_frame frame_header =
  let parse_fn length =
    lift2
      (fun s h -> Ok (PushPromiseFrame (s, h)))
      stream_identifier
      (take (length - 4))
  in
  parse_payload_with_padding frame_header parse_fn

let parse_ping_frame = lift (fun x -> Ok (PingFrame x)) (take 8)

let parse_go_away frame_header =
  lift3
    (fun s e x -> Ok (GoAwayFrame (s, e, x)))
    stream_identifier parse_error_code
    (take (frame_header.length - 8))

let parse_window_frame =
  lift
    (fun w ->
      let w' = clear_bit (Int32.to_int_exn w) 31 in
      if w' = 0 then
        Error (ConnectionError (ProtocolError, "window update must not be 0"))
      else Ok (WindowUpdateFrame w') )
    BE.any_int32

let get_parser_for_frame frame_header =
  match frame_header.frame_type with
  | FrameData -> parse_data_frame frame_header
  | FrameHeaders -> parse_header_frame frame_header
  | FramePriority -> parse_priority_frame
  | FrameRSTStream -> parse_rst_stream
  | FrameSettings -> parse_settings_frame frame_header
  | FramePushPromise -> parse_push_promise_frame frame_header
  | FramePing -> parse_ping_frame
  | FrameGoAway -> parse_go_away frame_header
  | FrameWindowUpdate -> parse_window_frame
  | _ -> failwith "not implemented yet"

let parse_frame settings =
  parse_frame_header
  >>= fun frame_header ->
  match check_frame_header settings frame_header with
  | Ok frame_header ->
      get_parser_for_frame frame_header
      >>= fun x ->
      return
        (Result.map x ~f:(fun frame_payload -> {frame_header; frame_payload}))
  | Error e -> return (Error e)

let%test "read frame" =
  let input = "\x01\x02\x03\x04\x05\x06\x07\x08\x09" in
  Caml.Pervasives.( = )
    (parse_string parse_frame_header input)
    (Ok
       { length = 66051
       ; frame_type = FrameSettings
       ; flags = 5
       ; stream_id = 101124105 })
