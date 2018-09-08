open Frames

type t = {frame : Types.frame}

let bad_request = `Error `Bad_request

let create frame = {frame}
