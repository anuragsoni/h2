open Frames
open Types

let wire = "00000403000000000500000008"

let wire2 = "000004030000000001000001a4"

let extract_payload = function
  | RSTStreamFrame e -> e
  | _ -> failwith "BAD PAYLOAD"

let parse_rst_frame () =
  let parsed = Util.parse_success wire in
  let error = extract_payload parsed.frame_payload in
  Alcotest.(check int) "Length" 4 parsed.frame_header.length ;
  Alcotest.(check int) "flags" 0 parsed.frame_header.flags ;
  Alcotest.(check int) "stream id" 5 parsed.frame_header.stream_id ;
  Alcotest.(check int) "type" 3 (frame_type_of_id parsed.frame_header.frame_type);
  Alcotest.(check int) "Error code" 8 (error_code_of_id error)

let parse_rst_frame' () =
  let parsed = Util.parse_success wire2 in
  let error = extract_payload parsed.frame_payload in
  Alcotest.(check int) "Length" 4 parsed.frame_header.length ;
  Alcotest.(check int) "flags" 0 parsed.frame_header.flags ;
  Alcotest.(check int) "stream id" 5 parsed.frame_header.stream_id ;
  Alcotest.(check int) "type" 3 (frame_type_of_id parsed.frame_header.frame_type);
  Alcotest.(check int) "Error code" 420 (error_code_of_id error)


let tests = [("Can parse rst stream header", `Quick, parse_rst_frame)]
