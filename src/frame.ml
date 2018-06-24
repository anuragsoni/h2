type t =
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

let t_to_id = function
  | Data -> 0x0
  | Headers -> 0x1
  | Priority -> 0x2
  | RSTStream -> 0x3
  | Settings -> 0x4
  | PushPromise -> 0x5
  | Ping -> 0x6
  | GoAway -> 0x7
  | WindowUpdate -> 0x8
  | Continuation -> 0x9

let t_of_id = function
  | 0x0 -> Ok Data
  | 0x1 -> Ok Headers
  | 0x2 -> Ok Priority
  | 0x3 -> Ok RSTStream
  | 0x4 -> Ok Settings
  | 0x5 -> Ok PushPromise
  | 0x6 -> Ok Ping
  | 0x7 -> Ok GoAway
  | 0x8 -> Ok WindowUpdate
  | 0x9 -> Ok Continuation
  | x -> Error ("Unknown value: " ^ string_of_int x)
