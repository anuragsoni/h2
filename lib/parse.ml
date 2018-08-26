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

let check_frame_header settings frame_header =
  let open Polymorphic_compare in
  let {enable_push; _} = settings in
  let {flags; length; frame_type; stream_id} = frame_header in
  let check_frame_type = function
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
    else lift parse_fn (take body_lenth)
  else lift parse_fn (take frame_header.length)

let parse_data_frame frame_header =
  parse_payload_with_padding frame_header (fun x -> Ok (DataFrame x))

let get_parser_for_frame = function
  | FrameData -> parse_data_frame
  | _ -> failwith "not implemented yet"

let parse_frame settings =
  parse_frame_header >>= (fun frame_header ->
    match check_frame_header settings frame_header with
    | Ok frame_header -> (get_parser_for_frame frame_header.frame_type) frame_header
    | Error e -> return (Error e)
  )
  (* lift (fun x -> check_frame_header settings x) parse_frame_header *)

let%test "read frame" =
  let input = "\x01\x02\x03\x04\x05\x06\x07\x08\x09" in
  Caml.Pervasives.( = )
    (parse_string parse_frame_header input)
    (Ok {length= 66051; frame_type= FrameSettings; flags= 5; stream_id= 101124105})
