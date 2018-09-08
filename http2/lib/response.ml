open Frames

type t = {frame: Types.frame}

let server_error = `Error `Internal_server_error

let create frame = {frame}
