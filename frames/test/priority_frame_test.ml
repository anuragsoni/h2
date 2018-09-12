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
  Alcotest.(check int32) "Stream id" 9l parsed.frame_header.stream_id ;
  Alcotest.(check int) "length" 5 parsed.frame_header.length ;
  Alcotest.(check int32) "Stream dependency" 11l payload.stream_dependency ;
  Alcotest.(check int) "Weight" 7 payload.weight ;
  Alcotest.(check bool) "Is exclusive" false payload.exclusive ;
  (* Second frame *)
  Alcotest.(check int) "Flags" 0 parsed2.frame_header.flags ;
  Alcotest.(check int32) "Stream id" 1l parsed2.frame_header.stream_id ;
  Alcotest.(check int) "Length" 5 parsed2.frame_header.length ;
  Alcotest.(check int32) "Stream dependency" 4l payload2.stream_dependency ;
  Alcotest.(check int) "Weight" 64 payload2.weight ;
  Alcotest.(check bool) "is exclusive" true payload2.exclusive

let serialize_priority_frame_1 () =
  let info = {Serialize.flags = 0; padding = None; stream_id = 9l} in
  let f = Faraday.create 5 in
  let priority = {exclusive = false; stream_dependency = 11l; weight = 7} in
  Frames.Serialize.write_frame f info (PriorityFrame priority) ;
  let res = Faraday.serialize_to_string f in
  Alcotest.(check string) "Serialized" wire (Util.hex_of_string res)

let serialize_priority_frame_2 () =
  let info = {Serialize.flags = 0; padding = None; stream_id = 1l} in
  let f = Faraday.create 5 in
  let priority = {exclusive = true; stream_dependency = 4l; weight = 64} in
  Frames.Serialize.write_frame f info (PriorityFrame priority) ;
  let res = Faraday.serialize_to_string f in
  Alcotest.(check string) "Serialized" wire2 (Util.hex_of_string res)

let tests =
  [ ("Can parse priority frame", `Quick, parse_priority_frame)
  ; ( "Can serialize priority frame payload wire"
    , `Quick
    , serialize_priority_frame_1 )
  ; ("Can serialize second priority frame", `Quick, serialize_priority_frame_2)
  ]
