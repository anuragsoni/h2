module type Comparable = sig
  type t

  type v

  val compare : t -> t -> int
end

module Make (K : Map.OrderedType) (V : Comparable) = struct
  module Q =
    Psq.Make
      (K)
      (struct
        type t = V.t * V.v

        let compare (t1, _) (t2, _) = V.compare t1 t2
      end)

  type t = Q.t

  let empty = Q.empty

  let is_empty = Q.is_empty

  let add k p v q = Q.add k (p, v) q

  let pop q =
    match Q.pop q with
    | None -> None
    | Some ((k, (p, v)), q') -> Some (k, p, v, q')

  let remove k q = Q.remove k q

  let find k q = Q.find k q

end
