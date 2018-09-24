open Base
open Types

let zero_frame_types = [FrameSettings; FramePing; FrameGoAway]

let non_zero_frame_types =
  [ FrameData
  ; FrameHeaders
  ; FramePriority
  ; FrameRSTStream
  ; FramePushPromise
  ; FrameContinuation ]

let check_frame_header settings frame_header frame_type =
  let open Polymorphic_compare in
  let {enable_push; _} = settings in
  let {flags; length; stream_id} = frame_header in
  let check_frame_type = function
    | FrameData when stream_id = 0x0l ->
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
    | FramePriority when length <> 5 ->
        Error (StreamError (FrameSizeError, stream_id))
    | FrameRSTStream when length <> 4 ->
        Error
          (ConnectionError
             (FrameSizeError, "payload length is not 4 in rst stream frame"))
    | FrameSettings when length % 6 <> 0 ->
        Error
          (ConnectionError
             ( FrameSizeError
             , "payload length is not multiple of 6 in settings frame" ))
    | FrameSettings when test_ack flags && length <> 0 ->
        Error
          (ConnectionError
             (FrameSizeError, "payload length must be 0 if ack flag is set"))
    | FramePushPromise when not enable_push ->
        Error (ConnectionError (ProtocolError, "push not enabled"))
    | FramePushPromise when not (is_response stream_id) ->
        Error
          (ConnectionError
             (ProtocolError, "push promise must be used with response streams"))
    | FramePing when length <> 8 ->
        Error
          (ConnectionError (FrameSizeError, "payload length is 8 in ping frame"))
    | FrameGoAway when length < 8 ->
        Error
          (ConnectionError
             (FrameSizeError, "go away body must be 8 bytes or more"))
    | FrameWindowUpdate when length <> 4 ->
        Error
          (ConnectionError
             (FrameSizeError, "payload length is 4 in window update frame"))
    | _ -> Ok {flags; length; stream_id}
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
