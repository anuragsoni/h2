open Base

(** A stream is an independent, bidirectional sequence of frames
    exchanged between the client and server within an HTTP/2 connection.
		See More at: {{: http://http2.github.io/http2-spec/#StreamsLayer} http://http2.github.io/http2-spec/#StreamsLayer}. *)

module State : sig
  (** The lifecycle of a HTTP/2 stream

    {[


                             +--------+
                     send PP |        | recv PP
                    ,--------|  idle  |--------.
                   /         |        |         \
                  v          +--------+          v
           +----------+          |           +----------+
           |          |          | send H /  |          |
    ,------| reserved |          | recv H    | reserved |------.
    |      | (local)  |          |           | (remote) |      |
    |      +----------+          v           +----------+      |
    |          |             +--------+             |          |
    |          |     recv ES |        | send ES     |          |
    |   send H |     ,-------|  open  |-------.     | recv H   |
    |          |    /        |        |        \    |          |
    |          v   v         +--------+         v   v          |
    |      +----------+          |           +----------+      |
    |      |   half   |          |           |   half   |      |
    |      |  closed  |          | send R /  |  closed  |      |
    |      | (remote) |          | recv R    | (local)  |      |
    |      +----------+          |           +----------+      |
    |           |                |                 |           |
    |           | send ES /      |       recv ES / |           |
    |           | send R /       v        send R / |           |
    |           | recv R     +--------+   recv R   |           |
    | send R /  `----------->|        |<-----------'  send R / |
    | recv R                 | closed |               recv R   |
    `----------------------->|        |<----------------------'
                             +--------+

       send:   endpoint sends this frame
       recv:   endpoint receives this frame

       H:  HEADERS frame (with implied CONTINUATIONs)
       PP: PUSH_PROMISE frame (with implied CONTINUATIONs)
       ES: END_STREAM flag
       R:  RST_STREAM frame
       ]} *)

  type state [@@deriving sexp]

  type action = SendPushPromise | RecvPushPromise | SendHeader | RecvHeader

  val create : state

  val is_idle : state -> bool

  val is_send_closed : state -> bool

  val is_recv_closed : state -> bool

  val is_closed : state -> bool

  val is_recv_streaming : state -> bool

  val can_recv_headers : state -> bool

  val is_send_streaming : state -> bool

  val is_reset : state -> bool

  val transition :
       state
    -> ?is_end_stream:bool
    -> action
    -> (unit, Frames.Types.http2_error) Result.t
end
