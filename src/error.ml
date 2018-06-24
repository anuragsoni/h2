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

let error_code_to_id = function
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
  | ConnectError -> 0xA
  | EnhanceYourCalm -> 0xB
  | InadequateSecurity -> 0xC
  | HTTP11Required -> 0xD

let error_code_of_id = function
  | 0x0 -> Ok NoError
  | 0x1 -> Ok ProtocolError
  | 0x2 -> Ok InternalError
  | 0x3 -> Ok FlowControlError
  | 0x4 -> Ok SettingsTimeout
  | 0x5 -> Ok StreamClosed
  | 0x6 -> Ok FrameSizeError
  | 0x7 -> Ok RefusedStream
  | 0x8 -> Ok Cancel
  | 0x9 -> Ok CompressionError
  | 0xA -> Ok ConnectError
  | 0xB -> Ok EnhanceYourCalm
  | 0xC -> Ok InadequateSecurity
  | 0xD -> Ok HTTP11Required
  | x -> Error ("Unknown value: " ^ string_of_int x)
