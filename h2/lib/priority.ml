open Base
open Frames

type key = int32

type deficit = int

let deficit_steps = 65536

let deficit_table =
  Array.init 256 ~f:(fun i ->
      Float.to_int
        (Float.round (Float.of_int deficit_steps /. Float.of_int (i + 1))) )

let deficit_of_weight w = deficit_table.(w - 1)

type precedence =
  {deficit : deficit; weight : Types.weight; dependency : Types.stream_id}

let compare {deficit = d1; _} {deficit = d2; _} = Int.compare d1 d2

(** TODO: remove me. used for test *)
let new_p w = {deficit = 0; weight = w; dependency = 0l}

module PriorityQueue = struct
  module Q =
    Pqueue.Make
      (Int32)
      (struct
        type t = precedence

        type v = int32

        let compare = compare
      end)

  type t = {base_deficit : deficit; queue : Q.t}

  let empty = {base_deficit = 0; queue = Q.empty}

  let is_empty {queue = q; _} = Q.is_empty q

  let add k ({weight; deficit; _} as p) v ({base_deficit; queue} as q) =
    let d = deficit_of_weight weight in
    let b = if deficit = 0 then base_deficit else deficit in
    let deficit' = Int.max (b + d) base_deficit in
    let p' = {p with deficit = deficit'} in
    let queue' = Q.add k p' v queue in
    {q with queue = queue'}

  let pop {queue; _} =
    match Q.pop queue with
    | None -> None
    | Some (k, p, v, queue') ->
        let base_deficit = p.deficit in
        Some (k, p, v, {base_deficit; queue = queue'})

  (** TODO: remove me. used for test *)
  let enqdeq queue num =
    let rec loop queue num acc =
      if num = 0 then acc
      else
        match pop queue with
        | None -> failwith "enqdeq"
        | Some (k, p, v, q') -> loop (add k p v q') (num - 1) (k :: acc)
    in
    loop queue num []
end
