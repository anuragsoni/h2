open H2
open Types

(* Payload with padding *)
let wire = "0000140008000000020648656C6C6F2C20776F726C6421486F77647921"

let wire_no_padding = "0000080001000000017465737464617461"

let extract_payload = function DataFrame x -> x | _ -> failwith "BAD PAYLOAD"

let parse_data_frame_with_padding () =
  let parsed = Util.parse_success wire in
  Alcotest.(check int) "Header flags" 8 parsed.frame_header.flags ;
  Alcotest.(check int) "Length" 20 parsed.frame_header.length ;
  Alcotest.(check int32) "StreamId" 2l parsed.frame_header.stream_id ;
  Alcotest.(check bool) "Padded" true (test_padded parsed.frame_header.flags) ;
  Alcotest.(check string)
    "Payload" "Hello, world!"
    (extract_payload parsed.frame_payload)

let parse_data_frame_no_padding () =
  let parsed = Util.parse_success wire_no_padding in
  Alcotest.(check int) "Length" 8 parsed.frame_header.length ;
  Alcotest.(check int) "Flags" 1 parsed.frame_header.flags ;
  Alcotest.(check int32) "StreamId" 1l parsed.frame_header.stream_id ;
  Alcotest.(check bool) "Padded" false (test_padded parsed.frame_header.flags) ;
  Alcotest.(check string)
    "Payload" "testdata"
    (extract_payload parsed.frame_payload)

let serialize_data_frame_with_padding () =
  let info = {Serialize.flags = 8; stream_id = 2l; padding = Some "Howdy!"} in
  let f = Faraday.create 20 in
  Serialize.write_frame f info (DataFrame "Hello, world!") ;
  let serialized = Faraday.serialize_to_string f in
  Alcotest.(check string)
    "Serialized data frame" wire
    (Util.hex_of_string serialized)

let serialize_data_frame_without_padding () =
  let info = {Serialize.flags = 1; stream_id = 1l; padding = None} in
  let f = Faraday.create 8 in
  Serialize.write_frame f info (DataFrame "testdata") ;
  let serialized = Faraday.serialize_to_string f in
  Alcotest.(check string)
    "Serialized data frame without padding" wire_no_padding
    (Util.hex_of_string serialized)

let tests =
  [ ( "Can parse dataframe payload with padding"
    , `Quick
    , parse_data_frame_with_padding )
  ; ( "Can parse dataframe payload without padding"
    , `Quick
    , parse_data_frame_no_padding )
  ; ( "Can serialize data frame with padding"
    , `Quick
    , serialize_data_frame_with_padding )
  ; ( "Can serialize data frame without padding"
    , `Quick
    , serialize_data_frame_without_padding ) ]
