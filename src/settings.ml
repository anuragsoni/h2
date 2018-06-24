type settings_identifier =
  | HeaderTableSize
  | EnablePush
  | MaxConcurrentStreams
  | InitialWindowSize
  | MaxFrameSize
  | MaxHeaderListSize

type settings_value = int

let settings_identifier_to_id = function
  | HeaderTableSize -> 0x1
  | EnablePush -> 0x2
  | MaxConcurrentStreams -> 0x3
  | InitialWindowSize -> 0x4
  | MaxFrameSize -> 0x5
  | MaxHeaderListSize -> 0x6

let settings_identifier_of_id = function
  | 0x1 -> Ok HeaderTableSize
  | 0x2 -> Ok EnablePush
  | 0x3 -> Ok MaxConcurrentStreams
  | 0x4 -> Ok InitialWindowSize
  | 0x5 -> Ok MaxFrameSize
  | 0x6 -> Ok MaxHeaderListSize
  | x -> Error ("Invalid settings value " ^ string_of_int x)

type settings_list = (settings_identifier * settings_value) list
