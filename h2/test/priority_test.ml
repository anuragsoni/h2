open H2

module P = Pqueue.Make (struct
  type v = int32
end)

let new_p w = {Pqueue.Priority.weight = w; deficit = 0}

let repeat queue num =
  let rec loop q n acc =
    if n = 0 then acc
    else
      match P.pop q with
      | None -> failwith "invalid queue"
      | Some (k, p, v, q') -> loop (P.add k p v q') (n - 1) (k :: acc)
  in
  loop queue num []

let test_priority_queue () =
  let q = P.empty in
  let q = P.add 1l (new_p 201) 1l q in
  let q = P.add 3l (new_p 101) 3l q in
  let q = P.add 5l (new_p 1) 5l q in
  Alcotest.(check bool) "Check if empty" false (P.is_empty q) ;
  let t = repeat q 1000 in
  let count_1 = List.filter (fun x -> x = 1l) t |> List.length in
  let count_3 = List.filter (fun x -> x = 3l) t |> List.length in
  let count_5 = List.filter (fun x -> x = 5l) t |> List.length in
  (* After multiple repetitions, the frequency of 1, 3 and 5 is proportional to
     their weight*)
  Alcotest.(check int) "Number of items with weight 1" 664 count_1 ;
  Alcotest.(check int) "Number of items with weight 3" 333 count_3 ;
  Alcotest.(check int) "Number of items with weight 5" 3 count_5

let tests = [("Priority queue can add/pop items", `Quick, test_priority_queue)]
