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

let frame_type_of_id = function
  | 0 -> Ok Data
  | 1 -> Ok Headers
  | 2 -> Ok Priority
  | 3 -> Ok RSTStream
  | 4 -> Ok Settings
  | 5 -> Ok PushPromise
  | 6 -> Ok Ping
  | 7 -> Ok GoAway
  | 8 -> Ok WindowUpdate
  | 9 -> Ok Continuation
  | x -> Error ("Unknown value: " ^ string_of_int x)
