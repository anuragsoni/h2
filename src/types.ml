let frame_header_size = 9

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
  | Unknown of int

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

let max_payload_length = pow 2 14

type frame_header = {payload_length: int; flags: int; stream_id: int}
