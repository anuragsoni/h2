open H2
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
  Alcotest.(check int32) "stream id" 5l parsed.frame_header.stream_id ;
  Alcotest.(check int32) "Error code" 8l (error_code_of_id error)

let parse_rst_frame' () =
  let parsed = Util.parse_success wire2 in
  let error = extract_payload parsed.frame_payload in
  Alcotest.(check int) "Length" 4 parsed.frame_header.length ;
  Alcotest.(check int) "flags" 0 parsed.frame_header.flags ;
  Alcotest.(check int32) "stream id" 1l parsed.frame_header.stream_id ;
  Alcotest.(check int32) "Error code" 420l (error_code_of_id error)

let serialize_rst_frame () =
  let f = Faraday.create 4 in
  let info = {Serialize.flags = 0; stream_id = 5l; padding = None} in
  let e = error_code_to_id 8l in
  Serialize.write_frame f info (RSTStreamFrame e) ;
  let res = Faraday.serialize_to_string f in
  Alcotest.(check string) "Serialized rst frame" wire (Util.hex_of_string res)

let serialize_rst_frame' () =
  let f = Faraday.create 4 in
  let info = {Serialize.flags = 0; stream_id = 1l; padding = None} in
  let e = error_code_to_id 420l in
  Serialize.write_frame f info (RSTStreamFrame e) ;
  let res = Faraday.serialize_to_string f in
  Alcotest.(check string) "Serialized rst frame" wire2 (Util.hex_of_string res)

let tests =
  [ ("Can parse rst stream frame", `Quick, parse_rst_frame)
  ; ("Second rst frame test", `Quick, parse_rst_frame')
  ; ("Serialize rst stream frame", `Quick, serialize_rst_frame)
  ; ("Serialize rst stream 2", `Quick, serialize_rst_frame') ]
