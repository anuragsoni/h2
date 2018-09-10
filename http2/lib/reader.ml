open Base
open Angstrom
module AU = Angstrom.Unbuffered
module FP = Frames.Parse
module FT = Frames.Types

type frame_error =
  [`Parse of string list * string | `HTTP2Error of FT.http2_error]

type 'error parse_state =
  | Done
  | Fail of 'error
  | Partial of
      (   Bigstringaf.t
       -> off:int
       -> len:int
       -> AU.more
       -> (unit, 'error) Result.t AU.state)

type 'error t =
  { parser : (unit, 'error) Result.t Angstrom.t
  ; mutable parse_state : 'error parse_state
  ; mutable closed : bool }

type frame = frame_error t

let create parser = {parser; parse_state = Done; closed = false}

let ok = return (Ok ())

let frame settings handler =
  let parser =
    FP.parse_frame settings
    >>= fun parsed ->
    match parsed with
    | Ok frame -> handler frame ; ok
    | Error e -> return (Error (`HTTP2Error e))
  in
  create parser
