open Base
open Frames

module rec PriorityTree : sig
  type t

  module Q : sig
    type t
  end

  val empty : t
end = struct
  module Q = Pqueue.Make (struct
    type v = Node.v
  end)

  type t =
    { nodes : (int32, Pqueue.precedence, Int32.comparator_witness) Map.t
    ; root : Node.v }

  let empty =
    let m = Map.empty (module Int32) in
    let nodes =
      Map.add_exn m ~key:0l
        ~data:(Pqueue.precedence_of_priority Types.default_priority)
    in
    let root =
      { Node.id = 0l
      ; precedence = Pqueue.precedence_of_priority Types.default_priority
      ; children = Q.empty }
    in
    {nodes; root}
end

and Node : sig
  type v =
    { id : Types.stream_id
    ; precedence : Pqueue.precedence
    ; children : PriorityTree.Q.t }
end = struct
  type v =
    { id : Types.stream_id
    ; precedence : Pqueue.precedence
    ; children : PriorityTree.Q.t }
end
