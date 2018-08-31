open Frames
open Types

let extract_payload = function
  | PriorityFrame f -> f
  | _ -> failwith "INVALID PAYLOAD"

let wire = "0000050200000000090000000B07"

let wire2 = "0000050200000000018000000440"

let parse_priority_frame () =
  let parsed = Util.parse_success wire in
  let payload = extract_payload parsed.frame_payload in
  let parsed2 = Util.parse_success wire2 in
  let payload2 = extract_payload parsed2.frame_payload in

  (* First frame *)
  Alcotest.(check int) "Flags" 0 parsed.frame_header.flags ;
  Alcotest.(check int) "Stream id" 9 parsed.frame_header.stream_id ;
  Alcotest.(check int)
    "Frame type" 2
    (frame_type_of_id parsed.frame_header.frame_type) ;
  Alcotest.(check int) "length" 5 parsed.frame_header.length ;
  Alcotest.(check int) "Stream dependency" 11 payload.stream_dependency ;
  Alcotest.(check int) "Weight" 7 payload.weight ;
  Alcotest.(check bool) "Is exclusive" false payload.exclusive;

  (* Second frame *)
  Alcotest.(check int) "Flags" 0 parsed2.frame_header.flags;
  Alcotest.(check int) "Stream id" 9 parsed.frame_header.stream_id;
  Alcotest.(check int) "Length" 5 parsed2.frame_header.length;
  Alcotest.(check int) "Weight" 64 payload2.weight;
  Alcotest.(check bool) "is exclusive" true payload2.exclusive

let tests = [("Can parse priority frame", `Quick, parse_priority_frame)]
