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

  type action = SendPushPromise | RecvPushPromise | SendHeader | RecvHeader

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

  let is_send_streaming state =
    match state.value with
    | Open {local = Streaming; _} -> true
    | HalfClosedRemote Streaming -> true
    | _ -> false

  let is_reset state =
    match state.value with
    | Closed EndStream -> false
    | Closed _ -> true
    | _ -> false

  let transition state ?(is_end_stream = false) action =
    match (state.value, action) with
    | Idle, SendPushPromise ->
        state.value <- ReservedLocal ;
        Ok ()
    | Idle, RecvPushPromise ->
        state.value <- ReservedRemote ;
        Ok ()
    | Idle, SendHeader when is_end_stream ->
        state.value <- HalfClosedLocal AwaitingHeaders ;
        Ok ()
    | Idle, SendHeader ->
        state.value <- Open {local = Streaming; remote = AwaitingHeaders} ;
        Ok ()
    | Idle, RecvHeader when is_end_stream ->
        state.value <- HalfClosedRemote AwaitingHeaders ;
        Ok ()
    | Idle, RecvHeader ->
        state.value <- Open {local = AwaitingHeaders; remote = Streaming} ;
        Ok ()
    | ReservedLocal, SendHeader ->
      state.value <- HalfClosedRemote(Streaming);
      Ok ()
end
