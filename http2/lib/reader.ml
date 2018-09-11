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

let close t = t.closed <- true

let is_closed t = t.closed

let transition t state =
  match state with
  | AU.Done (consumed, Ok ()) | AU.Fail ((0 as consumed), _, _) ->
      t.parse_state <- Done ;
      consumed
  | AU.Done (consumed, Error error) ->
      t.parse_state <- Fail error ;
      consumed
  | AU.Fail (consumed, marks, msg) ->
      t.parse_state <- Fail (`Parse (marks, msg)) ;
      consumed
  | AU.Partial {committed; continue} ->
      t.parse_state <- Partial continue ;
      committed

and start t state =
  match state with
  | AU.Done _ -> failwith "Unable to start parser"
  | AU.Fail (0, marks, msg) -> t.parse_state <- Fail (`Parse (marks, msg))
  | AU.Partial {committed = 0; continue} -> t.parse_state <- Partial continue
  | _ -> assert false

let rec read t bs ~off ~len =
  match t.parse_state with
  | Fail _ -> 0
  | Done ->
      start t (AU.parse t.parser) ;
      read t bs ~off ~len
  | Partial continue -> transition t (continue bs Incomplete ~off ~len)

let next t =
  match t.parse_state with
  | Done -> if t.closed then `Close else `Read
  | Fail _ -> `Close
  | Partial _ -> `Read
