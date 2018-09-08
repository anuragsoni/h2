open Frames
open Types

let wire = "00000403000000000500000008"

let wire2 = "000004030000000001000001A4"

let extract_payload = function
  | RSTStreamFrame e -> e
  | _ -> failwith "BAD PAYLOAD"

let parse_rst_frame () =
  let parsed = Util.parse_success wire in
  let error = extract_payload parsed.frame_payload in
  Alcotest.(check int) "Length" 4 parsed.frame_header.length ;
  Alcotest.(check int) "flags" 0 parsed.frame_header.flags ;
  Alcotest.(check int) "stream id" 5 parsed.frame_header.stream_id ;
  Alcotest.(check int) "Error code" 8 (error_code_of_id error)

let parse_rst_frame' () =
  let parsed = Util.parse_success wire2 in
  let error = extract_payload parsed.frame_payload in
  Alcotest.(check int) "Length" 4 parsed.frame_header.length ;
  Alcotest.(check int) "flags" 0 parsed.frame_header.flags ;
  Alcotest.(check int) "stream id" 1 parsed.frame_header.stream_id ;
  Alcotest.(check int) "Error code" 420 (error_code_of_id error)

let serialize_rst_frame () =
  let f = Faraday.create 4 in
  let info = {Serialize.flags = 0; stream_id = 5; padding = None} in
  let e = error_code_to_id 8 in
  Serialize.write_frame f info (RSTStreamFrame e) ;
  let res = Faraday.serialize_to_string f in
  Alcotest.(check string) "Serialized rst frame" wire (Util.hex_of_string res)

let serialize_rst_frame' () =
  let f = Faraday.create 4 in
  let info = {Serialize.flags = 0; stream_id = 1; padding = None} in
  let e = error_code_to_id 420 in
  Serialize.write_frame f info (RSTStreamFrame e) ;
  let res = Faraday.serialize_to_string f in
  Alcotest.(check string) "Serialized rst frame" wire2 (Util.hex_of_string res)

let tests =
  [ ("Can parse rst stream frame", `Quick, parse_rst_frame)
  ; ("Second rst frame test", `Quick, parse_rst_frame')
  ; ("Serialize rst stream frame", `Quick, serialize_rst_frame)
  ; ("Serialize rst stream 2", `Quick, serialize_rst_frame') ]
