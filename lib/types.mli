val test_bit : int -> int -> bool

val set_bit : int -> int -> int

val complement_bit : int -> int -> int

val clear_bit : int -> int -> int

val frame_header_length : int

val max_payload_length : int

type stream_id = int

(** Error Codes. See: {{: http://http2.github.io/http2-spec/#ErrorCodes}
    http://http2.github.io/http2-spec/#ErrorCodes *)
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

val error_code_of_id : error_code_id -> error_code

val error_code_to_id : error_code -> error_code_id

type http2_error =
  | ConnectionError of error_code_id * string
  | StreamError of error_code_id * stream_id

val error_code_id_of_http : http2_error -> error_code_id

(** HTTP/2 Settings Registry. See: {{:
    http://http2.github.io/http2-spec/#iana-settings}
    http://http2.github.io/http2-spec/#iana-settings} *)
type settings_key_id =
  | SettingsHeaderTableSize
  | SettingsEnablePush
  | SettingsMaxConcurrentStreams
  | SettingsInitialWindowSize
  | SettingsMaxFrameSize
  | SettingsMaxHeaderListSize

type window_size = int

type settings_value = int

val settings_key_from_id : settings_key_id -> int

val settings_key_to_id : int -> settings_key_id option

val default_initial_window_size : window_size

val max_window_size : window_size

val is_window_overflow : window_size -> bool

type settings_list = (settings_key_id * settings_value) list

type settings =
  { header_table_size : int
  ; enable_push : bool
  ; max_concurrent_streams : int option
  ; initial_window_size : window_size
  ; max_frame_size : int
  ; max_header_list_size : int option }

val default_settings : settings

val check_settings_value : settings_key_id * settings_value -> http2_error option

val check_settings_list : settings_list -> http2_error option

val update_settings : settings -> settings_list -> settings

type weight = int

type priority = {exclusive : bool; stream_dependency : stream_id; weight : weight}

(** Default priority for all streams. See: {{:
    http://http2.github.io/http2-spec/#pri-default}
    http://http2.github.io/http2-spec/#pri-default} *)
val default_priority : priority

(** Maximum priority for any stream. See:
    {{:http://http2.github.io/http2-spec/#rfc.section.5.3.2}
    http://http2.github.io/http2-spec/#rfc.section.5.3.2} *)
val highest_priority : priority

(** The HTTP/2 frame type. See: {{:
    http://http2.github.io/http2-spec/#rfc.section.11.2}
    http://http2.github.io/http2-spec/#rfc.section.11.2} *)
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

val frame_type_of_id : frame_type_id -> frame_type

val frame_type_to_id : frame_type -> frame_type_id

val frame_type_id_to_name : frame_type_id -> string

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

val has_flag : frame_flags -> frame_flags -> bool

val flag_type_to_id : flag_type -> int

val flag_type_to_name : flag_type -> string

val flags_for_frame_type_id : frame_type_id -> flag_type list

val default_flags : frame_flags

val test_end_stream : frame_flags -> bool

val test_end_header : frame_flags -> bool

val test_ack : frame_flags -> bool

val test_padded : frame_flags -> bool

val test_priority : frame_flags -> bool

val set_end_stream : frame_flags -> frame_flags

val set_ack : frame_flags -> frame_flags

val set_end_header : frame_flags -> frame_flags

val set_padded : frame_flags -> frame_flags

val set_priority : frame_flags -> frame_flags

val is_control : stream_id -> bool

val is_request : stream_id -> bool

val is_response : stream_id -> bool

val test_exclusive : stream_id -> bool

val set_exclusive : stream_id -> stream_id

val clear_exclusive : stream_id -> stream_id

(** FrameHeader is the 9 byte header of all HTTP/2 frames. See:
    {{:http://http2.github.io/http2-spec/#FrameHeader}
    http://http2.github.io/http2-spec/#FrameHeader}*)
type frame_header =
  { length : int
  ; frame_type : frame_type_id
  ; flags : frame_flags
  ; stream_id : stream_id }

type data_frame = string

type frame_payload = DataFrame of data_frame | PriorityFrame of priority

type frame = {frame_header : frame_header; frame_payload : frame_payload}
