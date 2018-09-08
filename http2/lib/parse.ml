open Base
open Angstrom

let extract_frame settings ~f =
  lift (fun parsed -> Result.map ~f parsed) (Frames.Parse.parse_frame settings)

let request settings = extract_frame settings ~f:Request.create

let response settings = extract_frame settings ~f:Response.create

module Reader = struct
  module AU = Angstrom.Unbuffered

  type request_error =
    [ `Bad_request of Request.t
    | `H2Error of Frames.Types.http2_error
    | `Parse of string list * string ]

  type response_error =
    [`Parse of string list * string | `H2Error of Frames.Types.http2_error]

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

  type request = request_error t

  type response = response_error t

  let create parser = {parser; parse_state = Done; closed = false}

  let ok = return (Ok ())

  let request settings handler =
    let parser =
      request settings
      >>= fun request ->
      match request with
      | Ok request -> handler request ; ok
      | Error e -> return (Error (`H2Error e))
    in
    create parser

  let response settings handler =
    let parser =
      response settings
      >>= fun request ->
      match request with
      | Ok response -> handler response ; ok
      | Error e -> return (Error (`H2Error e))
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
    | AU.Done _ -> failwith "unable to start parser"
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
end
