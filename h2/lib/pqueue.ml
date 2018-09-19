open Frames

type deficit = int

let deficit_steps = 65536

let deficit_table =
  Array.init 256 (fun i ->
      let open Base in
      Float.to_int
        (Float.round (Float.of_int deficit_steps /. Float.of_int (i + 1))) )

let deficit_of_weight w = deficit_table.(w - 1)

type precedence =
  {deficit : deficit; weight : Types.weight; dependency : Types.stream_id}

let precedence_of_priority {Types.stream_dependency; weight; _} =
  {deficit = 0; weight; dependency = stream_dependency}

let compare_precedence {deficit = d1; _} {deficit = d2; _} =
  compare (d1 : int) d2

(** TODO: remove me. used for test *)
let new_p w = {deficit = 0; weight = w; dependency = 0l}

module Make (V : sig
  type v
end) =
struct
  module Q =
    Psq.Make
      (Int32)
      (struct
        type t = precedence * V.v

        let compare (t1, _) (t2, _) = compare_precedence t1 t2
      end)

  type t = {base_deficit : deficit; queue : Q.t}

  let empty = {base_deficit = 0; queue = Q.empty}

  let is_empty {queue = q; _} = Q.is_empty q

  let add k ({weight; deficit; _} as p) v ({base_deficit; queue} as q) =
    let d = deficit_of_weight weight in
    let b = if deficit = 0 then base_deficit else deficit in
    let deficit' = max (b + d) base_deficit in
    let p' = {p with deficit = deficit'} in
    let queue' = Q.add k (p', v) queue in
    {q with queue = queue'}

  let pop {queue; _} =
    match Q.pop queue with
    | None -> None
    | Some ((k, (p, v)), q') ->
        let base_deficit = p.deficit in
        Some (k, p, v, {base_deficit; queue = q'})

  let remove k q =
    let f = Q.find k q.queue in
    match f with
    | None -> (None, q)
    | Some (_, v) -> (
      match Q.min q.queue with
      | None -> failwith "can't find min value"
      | Some (k', (p', _)) ->
          if k' = k then
            (* We removed the min element. Update base_deficit just like we do in
               [pop] *)
            (Some v, {base_deficit = p'.deficit; queue = Q.remove k q.queue})
          else (Some v, {q with queue = Q.remove k q.queue}) )

  (* TODO: (remove me) used for testing *)
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
