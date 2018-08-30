open Frames
open Types

(* Payload with padding *)
let wire = "0000140008000000020648656C6C6F2C20776F726C6421486F77647921"

let wire_no_padding = "0000080001000000017465737464617461"

let extract_payload = function DataFrame x -> x | _ -> failwith "BAD PAYLOAD"

let expected_frame =
  { frame_header = {length = 20; frame_type = FrameData; flags = 8; stream_id = 2}
  ; frame_payload = DataFrame "Hello, world!" }

let parse_data_frame_with_padding () =
  let parsed = Util.parse_success wire in
  Alcotest.(check int) "Header flags" 8 parsed.frame_header.flags ;
  Alcotest.(check int) "Length" 20 parsed.frame_header.length ;
  Alcotest.(check int) "StreamId" 2 parsed.frame_header.stream_id ;
  Alcotest.(check bool) "Padded" true (test_padded parsed.frame_header.flags);
  Alcotest.(check string)
    "Payload" "Hello, world!"
    (extract_payload parsed.frame_payload)

let parse_data_frame_no_padding () =
  let parsed = Util.parse_success wire_no_padding in
  Alcotest.(check int) "Length" 8 parsed.frame_header.length ;
  Alcotest.(check int) "Flags" 1 parsed.frame_header.flags ;
  Alcotest.(check int) "StreamId" 1 parsed.frame_header.stream_id ;
  Alcotest.(check bool) "Padded" false (test_padded parsed.frame_header.flags);
  Alcotest.(check string)
    "Payload" "testdata"
    (extract_payload parsed.frame_payload)

let tests =
  [ ( "Can parse dataframe payload with padding"
    , `Quick
    , parse_data_frame_with_padding )
  ; ( "Can parse dataframe payload without padding"
    , `Quick
    , parse_data_frame_no_padding ) ]
