open Base

(* Utilities *)

let test_bit x i = not (Int.equal (x land (1 lsl i)) 0)

let set_bit x i = x lor (1 lsl i)

let complement_bit x i = x lxor (1 lsl i)

let clear_bit x i = x land lnot (1 lsl i)

(* Constants *)

let frame_header_length = 9

let max_payload_length = Int.pow 2 14

(* Stream identifer *)

type stream_id = int

(* Errors *)

type error_code = int

type error_code_id =
  | NoError
  | ProtocolError
  | InternalError
  | FlowControlError
  | SettingsTimeout
  | StreamClosed
  | FrameSizeError
  | RefusedStream
  | Cancel
  | CompressionError
  | ConnectError
  | EnhanceYourCalm
  | InadequateSecurity
  | HTTP11Required
  | UnknownErrorCode of int

let error_code_of_id = function
  | NoError -> 0x0
  | ProtocolError -> 0x1
  | InternalError -> 0x2
  | FlowControlError -> 0x3
  | SettingsTimeout -> 0x4
  | StreamClosed -> 0x5
  | FrameSizeError -> 0x6
  | RefusedStream -> 0x7
  | Cancel -> 0x8
  | CompressionError -> 0x9
  | ConnectError -> 0xa
  | EnhanceYourCalm -> 0xb
  | InadequateSecurity -> 0xc
  | HTTP11Required -> 0xd
  | UnknownErrorCode x -> x

let error_code_to_id = function
  | 0x0 -> NoError
  | 0x1 -> ProtocolError
  | 0x2 -> InternalError
  | 0x3 -> FlowControlError
  | 0x4 -> SettingsTimeout
  | 0x5 -> StreamClosed
  | 0x6 -> FrameSizeError
  | 0x7 -> RefusedStream
  | 0x8 -> Cancel
  | 0x9 -> CompressionError
  | 0xa -> ConnectError
  | 0xb -> EnhanceYourCalm
  | 0xc -> InadequateSecurity
  | 0xd -> HTTP11Required
  | w -> UnknownErrorCode w

type http2_error =
  | ConnectionError of error_code_id * string
  | StreamError of error_code_id * stream_id

let error_code_id_of_http = function
  | ConnectionError (err, _) -> err
  | StreamError (err, _) -> err

(** HTTP/2 Settings key *)

type settings_key_id =
  | SettingsHeaderTableSize
  | SettingsEnablePush
  | SettingsMaxConcurrentStreams
  | SettingsInitialWindowSize
  | SettingsMaxFrameSize
  | SettingsMaxHeaderListSize

type window_size = int

type settings_value = int

let settings_key_from_id = function
  | SettingsHeaderTableSize -> 0x1
  | SettingsEnablePush -> 0x2
  | SettingsMaxConcurrentStreams -> 0x3
  | SettingsInitialWindowSize -> 0x4
  | SettingsMaxFrameSize -> 0x5
  | SettingsMaxHeaderListSize -> 0x6

let settings_key_to_id = function
  | 0x1 -> Some SettingsHeaderTableSize
  | 0x2 -> Some SettingsEnablePush
  | 0x3 -> Some SettingsMaxConcurrentStreams
  | 0x4 -> Some SettingsInitialWindowSize
  | 0x5 -> Some SettingsMaxFrameSize
  | 0x6 -> Some SettingsMaxHeaderListSize
  | _ -> None

let default_initial_window_size = 65535

let max_window_size = 2147483647

let is_window_overflow w = test_bit w 31

type settings_list = (settings_key_id * settings_value) list

type settings =
  { header_table_size : int
  ; enable_push : bool
  ; max_concurrent_streams : int option
  ; initial_window_size : window_size
  ; max_frame_size : int
  ; max_header_list_size : int option }

let default_settings =
  { header_table_size = 4096
  ; enable_push = true
  ; max_concurrent_streams = None
  ; initial_window_size = default_initial_window_size
  ; max_frame_size = 16384
  ; max_header_list_size = None }

let check_settings_value = function
  | SettingsEnablePush, v ->
      if (not (Int.equal v 0)) && not (Int.equal v 1) then
        Some (ConnectionError (ProtocolError, "enable push must be 0 or 1"))
      else None
  | SettingsInitialWindowSize, v ->
      if v > 2147483647 then
        Some
          (ConnectionError
             (FlowControlError, "Window size must be less than or equal to 65535"))
      else None
  | SettingsMaxFrameSize, v ->
      if v < 16395 || v > 16777215 then
        Some
          (ConnectionError
             ( ProtocolError
             , "Max frame size must be in between 16384 and 16777215" ))
      else None
  | _ -> None

let check_settings_list settings =
  let results = List.filter_map ~f:check_settings_value settings in
  match results with [] -> None | x :: _ -> Some x

let update_settings settings kvs =
  let update settings = function
    | SettingsHeaderTableSize, v -> {settings with header_table_size = v}
    | SettingsEnablePush, v -> {settings with enable_push = v > 0}
    | SettingsMaxConcurrentStreams, v ->
        {settings with max_concurrent_streams = Some v}
    | SettingsInitialWindowSize, v -> {settings with initial_window_size = v}
    | SettingsMaxFrameSize, v -> {settings with max_frame_size = v}
    | SettingsMaxHeaderListSize, v ->
        {settings with max_header_list_size = Some v}
  in
  List.fold_left kvs ~init:settings ~f:update

type weight = int

type priority = {exclusive : bool; stream_dependency : stream_id; weight : weight}

let default_priority = {exclusive = false; stream_dependency = 0; weight = 16}

let highest_priority = {exclusive = false; stream_dependency = 0; weight = 256}

(* Raw HTTP/2 frame types *)

type frame_type = int

type frame_type_id =
  | FrameData
  | FrameHeaders
  | FramePriority
  | FrameRSTStream
  | FrameSettings
  | FramePushPromise
  | FramePing
  | FrameGoAway
  | FrameWindowUpdate
  | FrameContinuation
  | FrameUnknown of int

let frame_type_of_id = function
  | FrameData -> 0x0
  | FrameHeaders -> 0x1
  | FramePriority -> 0x2
  | FrameRSTStream -> 0x3
  | FrameSettings -> 0x4
  | FramePushPromise -> 0x5
  | FramePing -> 0x6
  | FrameGoAway -> 0x7
  | FrameWindowUpdate -> 0x8
  | FrameContinuation -> 0x9
  | FrameUnknown x -> x

let frame_type_to_id = function
  | 0x0 -> FrameData
  | 0x1 -> FrameHeaders
  | 0x2 -> FramePriority
  | 0x3 -> FrameRSTStream
  | 0x4 -> FrameSettings
  | 0x5 -> FramePushPromise
  | 0x6 -> FramePing
  | 0x7 -> FrameGoAway
  | 0x8 -> FrameWindowUpdate
  | 0x9 -> FrameContinuation
  | id -> FrameUnknown id

let frame_type_id_to_name = function
  | FrameData -> "DATA"
  | FrameHeaders -> "HEADERS"
  | FramePriority -> "PRIORITY"
  | FrameRSTStream -> "RST_STREAM"
  | FrameSettings -> "SETTINGS"
  | FramePushPromise -> "PUSH_PROMISE"
  | FramePing -> "PING"
  | FrameGoAway -> "GOAWAY"
  | FrameWindowUpdate -> "WINDOW_UPDATE"
  | FrameContinuation -> "CONTINUATION"
  | FrameUnknown _ -> "UNKNOWN"

(* Flags *)

type frame_flags = int

type flag_type =
  | FlagDataEndStream
  | FlagDataPadded
  | FlagHeadersEndStream
  | FlagHeadersEndHeaders
  | FlagHeadersPadded
  | FlagHeadersPriority
  | FlagSettingsAck
  | FlagPingAck
  | FlagContinuationEndHeaders
  | FlagPushPromiseEndHeaders
  | FlagPushPromisePadded

let has_flag t flag = t land flag = flag

let flag_type_to_id = function
  | FlagDataEndStream -> 0x1
  | FlagDataPadded -> 0x8
  | FlagHeadersEndStream -> 0x1
  | FlagHeadersEndHeaders -> 0x4
  | FlagHeadersPadded -> 0x8
  | FlagHeadersPriority -> 0x20
  | FlagSettingsAck -> 0x1
  | FlagPingAck -> 0x1
  | FlagContinuationEndHeaders -> 0x4
  | FlagPushPromiseEndHeaders -> 0x4
  | FlagPushPromisePadded -> 0x8

let flag_type_to_name = function
  | FlagDataEndStream -> "END_STREAM"
  | FlagDataPadded -> "PADDED"
  | FlagHeadersEndStream -> "END_STREAM"
  | FlagHeadersEndHeaders -> "END_HEADERS"
  | FlagHeadersPadded -> "PADDED"
  | FlagHeadersPriority -> "PRIORITY"
  | FlagSettingsAck -> "ACK"
  | FlagPingAck -> "ACK"
  | FlagContinuationEndHeaders -> "END_HEADERS"
  | FlagPushPromiseEndHeaders -> "END_HEADERS"
  | FlagPushPromisePadded -> "PADDED"

let flags_for_frame_type_id = function
  | FrameData -> [FlagDataEndStream; FlagDataPadded]
  | FrameHeaders ->
      [ FlagHeadersEndStream
      ; FlagHeadersEndHeaders
      ; FlagHeadersPadded
      ; FlagHeadersPriority ]
  | FrameSettings -> [FlagSettingsAck]
  | FramePing -> [FlagPingAck]
  | FrameContinuation -> [FlagContinuationEndHeaders]
  | FramePushPromise -> [FlagPushPromiseEndHeaders; FlagPushPromisePadded]
  | _ -> []

let default_flags = 0

let test_end_stream x = test_bit x 0

let test_ack x = test_bit x 0

let test_end_header x = test_bit x 2

let test_padded x = test_bit x 3

let test_priority x = test_bit x 5

let set_end_stream x = set_bit x 0

let set_ack x = set_bit x 0

let set_end_header x = set_bit x 2

let set_padded x = set_bit x 3

let set_priority x = set_bit x 5

(* Streams *)

let is_control id = id = 0

let is_request id = id % 2 = 1

let is_response id = if id = 0 then false else id % 2 = 0

let test_exclusive id = test_bit id 31

let set_exclusive id = set_bit id 31

let clear_exclusive id = clear_bit id 31

(* HTTP/2 frame types *)

type data_frame = string

type frame_header =
  { length : int
  ; frame_type : frame_type_id
  ; flags : frame_flags
  ; stream_id : stream_id }

type frame_payload = DataFrame of data_frame
                   | HeadersFrame of priority option * string
                   | PriorityFrame of priority
                   | RSTStreamFrame of error_code_id
                   | SettingsFrame of settings_list
                   | PushPromiseFrame of stream_id * string

type frame = {frame_header : frame_header; frame_payload : frame_payload}
