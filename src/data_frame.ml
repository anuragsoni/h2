type t = {data: string; stream_id: int; flags: int; padding_length: int option}
[@@deriving fields]

let end_stream = 0x1

let padded = 0x8
