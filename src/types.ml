type frame_type =
  | Data
  | Headers
  | Priority
  | RSTStream
  | Settings
  | PushPromise
  | Ping
  | GoAway
  | WindowUpdate
  | Continuation
  | Unknown of int

let frame_type_to_id = function
  | Data -> 0
  | Headers -> 1
  | Priority -> 2
  | RSTStream -> 3
  | Settings -> 4
  | PushPromise -> 5
  | Ping -> 6
  | GoAway -> 7
  | WindowUpdate -> 8
  | Continuation -> 9
  | Unknown x -> x

let frame_type_of_id = function
  | 0 -> Data
  | 1 -> Headers
  | 2 -> Priority
  | 3 -> RSTStream
  | 4 -> Settings
  | 5 -> PushPromise
  | 6 -> Ping
  | 7 -> GoAway
  | 8 -> WindowUpdate
  | 9 -> Continuation
  | x -> Unknown x

type error_code_id = int

type error_code =
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
  | Unknown of error_code_id

let error_code_to_id = function
  | NoError -> 0
  | ProtocolError -> 1
  | InternalError -> 2
  | FlowControlError -> 3
  | SettingsTimeout -> 4
  | StreamClosed -> 5
  | FrameSizeError -> 6
  | RefusedStream -> 7
  | Cancel -> 8
  | CompressionError -> 9
  | ConnectError -> 10
  | EnhanceYourCalm -> 11
  | InadequateSecurity -> 12
  | HTTP11Required -> 13
  | Unknown x -> x

let error_code_of_id = function
  | 0 -> NoError
  | 1 -> ProtocolError
  | 2 -> InternalError
  | 3 -> FlowControlError
  | 4 -> SettingsTimeout
  | 5 -> StreamClosed
  | 6 -> FrameSizeError
  | 7 -> RefusedStream
  | 8 -> Cancel
  | 9 -> CompressionError
  | 10 -> ConnectError
  | 11 -> EnhanceYourCalm
  | 12 -> InadequateSecurity
  | 13 -> HTTP11Required
  | x -> Unknown x

let pow a b = a lsl (b - 1)

(* Frame size: https://httpwg.org/specs/rfc7540.html#rfc.section.4.2 *)
type header_block_fragment = bytes

type stream_id = int

let max_payload_length = pow 2 24 - 1

let frame_header_size = 9

type frame_header = {payload_length: int; flags: int; stream_id: stream_id}

(* Frame definitions https://httpwg.org/specs/rfc7540.html#rfc.section.6 *)

(* https://httpwg.org/specs/rfc7540.html#rfc.section.6.3 *)
type weight = int
type priority =
  {exclusive_dependency: bool; stream_dependency: stream_id; weight: weight}

(* https://httpwg.org/specs/rfc7540.html#rfc.section.6.2 *)
type headers_frame =
  {priority: priority option; header_block_fragment: header_block_fragment}

(* https://httpwg.org/specs/rfc7540.html#rfc.section.6.4 *)
type rst_stream = error_code_id

(* https://httpwg.org/specs/rfc7540.html#rfc.section.6.5 *)
type settings_identifier =
  | HeaderTableSize
  | EnablePush
  | MaxConcurrentStreams
  | InitialWindowSize
  | MaxFrameSize
  | MaxHeaderListSize

type settings_value = int

let settings_identifier_to_int = function
  | HeaderTableSize -> 1
  | EnablePush -> 2
  | MaxConcurrentStreams -> 3
  | InitialWindowSize -> 4
  | MaxFrameSize -> 5
  | MaxHeaderListSize -> 6

let int_to_settings_identifier = function
  | 1 -> HeaderTableSize
  | 2 -> EnablePush
  | 3 -> MaxConcurrentStreams
  | 4 -> InitialWindowSize
  | 5 -> MaxFrameSize
  | 6 -> MaxHeaderListSize
  | _ -> failwith "Invalid value for settings identifier"

type settings_list = (settings_identifier * settings_value) list

(* https://httpwg.org/specs/rfc7540.html#rfc.section.6.9 *)
type window_size = int

type frame_payload =
  | DataFrame of bytes
  | HeadersFrame of headers_frame
  | RSTStreamFrame of rst_stream
  | SettingsFrame of settings_list
  | PushPromiseFrame of stream_id * header_block_fragment
  | PingFrame of bytes
  | GoAwayFrame of stream_id * error_code_id * bytes
  | WindowUpdateFrame of window_size
