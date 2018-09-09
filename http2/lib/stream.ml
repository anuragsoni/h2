open Base
open Frames

module State = struct
  type peer = AwaitingHeaders | Streaming [@@deriving sexp]

  type cause = EndStream | LocallyReset of Types.error_code [@@deriving sexp]

  type t =
    | Idle
    | ReservedRemote
    | ReservedLocal
    | Open of {local : peer; remote : peer}
    | HalfClosedRemote of peer
    | HalfClosedLocal of peer
    | Closed of cause
  [@@deriving sexp]

  type state = {mutable value : t} [@@deriving sexp]

  let create = {value = Idle}

  let is_idle state = match state.value with Idle -> true | _ -> false

  let is_send_closed state =
    match state.value with
    | Closed _ | HalfClosedLocal _ | ReservedRemote -> true
    | _ -> false

  let is_recv_closed state =
    match state.value with
    | Closed _ | ReservedLocal | HalfClosedRemote _ -> true
    | _ -> false

  let is_closed state = match state.value with Closed _ -> true | _ -> false

  let is_recv_streaming state =
    match state.value with
    | Open {remote = Streaming; _} -> true
    | HalfClosedLocal Streaming -> true
    | _ -> false

  let can_recv_headers state =
    match state.value with
    | Idle -> true
    | Open {remote = AwaitingHeaders; _} -> true
    | HalfClosedLocal AwaitingHeaders -> true
    | ReservedRemote -> true
    | _ -> false
end