module Tree = struct
  module IntMap = Map.Make (Int32)

  module Q = Pqueue.Make (struct
    type v = int32
  end)

  type node =
    {dependency : Types.stream_id; priority : Pqueue.Priority.t; children : Q.t}

  let create_node ~parent ~priority =
    {dependency = parent; priority; children = Q.empty}

  type t = node IntMap.t

  let empty =
    let n =
      create_node ~parent:0l ~priority:(Pqueue.Priority.create ~weight:16)
    in
    IntMap.add 0l n IntMap.empty

  let append_child ~stream ~child ~weight t =
    IntMap.update stream
      (fun x ->
        match x with
        | None -> None
        | Some node ->
            Some
              { node with
                children =
                  Q.add child
                    (Pqueue.Priority.create ~weight)
                    child node.children } )
      t

  let add ~stream ~parent ~weight t =
    let m =
      match IntMap.find_opt parent t with
      | None ->
          (* parent not found. Inserting in root node's queue *)
          append_child ~stream:0l ~child:stream ~weight t
      | Some _ -> append_child ~stream:parent ~child:stream ~weight t
    in
    IntMap.add stream
      (create_node ~parent ~priority:(Pqueue.Priority.create ~weight))
      m
end
