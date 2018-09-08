open Faraday

let write_request t info {Request.frame} =
  Frames.Serialize.write_frame t info frame.frame_payload

let write_response t info {Response.frame} =
  Frames.Serialize.write_frame t info frame.frame_payload

module Writer = struct
  type t =
    {buffer : Bigstringaf.t; encoder : Faraday.t; mutable drained_bytes : int}

  let create ?(buffer_size = 0x800) () =
    let buffer = Bigstringaf.create buffer_size in
    let encoder = Faraday.of_bigstring buffer in
    {buffer; encoder; drained_bytes = 0}

  let faraday t = t.encoder

  let write_request t request info = write_request t.encoder request info

  let write_response t response info = write_response t.encoder response info

  let write_string t ?off ?len string = write_string t.encoder ?off ?len string

  let write_bytes t ?off ?len bytes = write_bytes t.encoder ?off ?len bytes

  let write_bigstring t ?off ?len bigstring =
    write_bigstring t.encoder ?off ?len bigstring
end
