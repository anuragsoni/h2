module Priority : sig
  type deficit = int

  type weight = int

  type t = {deficit : deficit; weight : weight}

  val compare : t -> t -> int

  val deficit_of_weight : weight -> deficit
end = struct
  type deficit = int

  type weight = int

  type t = {deficit : deficit; weight : weight}

  let compare {deficit = d1; _} {deficit = d2; _} = compare (d1 : int) d2

  let steps = 65536

  let deficit_table =
    let round x = floor (x +. 0.5) in
    Array.init 256 (fun i ->
        Float.to_int (round (Float.of_int steps /. Float.of_int (i + 1))) )

  let deficit_of_weight w = deficit_table.(w - 1)
end

module Make (V : sig
  type v
end) =
struct
  module Q =
    Psq.Make
      (Int32)
      (struct
        type t = Priority.t * V.v

        let compare (p1, _) (p2, _) = Priority.compare p1 p2
      end)

  type t = {base_deficit : Priority.deficit; queue : Q.t}

  let empty = {base_deficit = 0; queue = Q.empty}

  let is_empty {queue; _} = Q.is_empty queue

  let size {queue; _} = Q.size queue

  let add k ({Priority.weight; deficit} as p) v ({base_deficit; queue} as q) =
    let d = Priority.deficit_of_weight weight in
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
end
