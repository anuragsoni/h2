open Frames
open Faraday

type t =
  {buffer : Bigstringaf.t; encoder : Faraday.t; mutable drained_bytes : int}

let create ?(buffer_size = 0x800) () =
  let buffer = Bigstringaf.create buffer_size in
  let encoder = Faraday.of_bigstring buffer in
  {buffer; encoder; drained_bytes = 0}

let faraday t = t.encoder

let write_frame t frame info = Serialize.write_frame t.encoder info frame

let flush t f = flush t.encoder f

let close t = Faraday.close t.encoder

let close_and_drain t =
  Faraday.close t.encoder ;
  let drained = Faraday.drain t.encoder in
  t.drained_bytes <- t.drained_bytes + drained

let is_closed t = Faraday.is_closed t.encoder

let drained_bytes t = t.drained_bytes

let report_result t result =
  match result with `Closed -> close t | `Ok len -> shift t.encoder len

let next t =
  match Faraday.operation t.encoder with
  | `Close -> `Close (drained_bytes t)
  | `Yield -> `Yield
  | `Writev iovecs -> `Write iovecs
