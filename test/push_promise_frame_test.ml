open Http2

let wire = "000018050C0000000A060000000C746869732069732064756D6D79486F77647921"

let wire' = "000011050c0000000a0000000c746869732069732064756d6d79"

let extract_payload payload =
  let open Types in
  match payload with
  | PushPromiseFrame (s, m) -> (s, m)
  | _ -> failwith "INVALID FRAME"

let parse_push_promise_frame () =
  let parsed = Util.parse_success wire in
  Alcotest.(check int) "Flags" 12 parsed.frame_header.flags ;
  Alcotest.(check int) "Stream id" 10 parsed.frame_header.stream_id ;
  Alcotest.(check int) "Length" 24 parsed.frame_header.length ;
  let stream, message = extract_payload parsed.frame_payload in
  Alcotest.(check int) "Stream id" 12 stream ;
  Alcotest.(check string) "message" "this is dummy" message

let serialize_push_promise_frame_with_padding () =
  let info = {Serialize.flags = 12; stream_id = 10; padding = Some "Howdy!"} in
  let f = Faraday.create 24 in
  Serialize.write_frame f info
    (Types.PushPromiseFrame (12, "this is dummy")) ;
  let output = Faraday.serialize_to_string f in
  Alcotest.(check string) "Serialized" (Util.string_of_hex wire) output

let serialize_push_promise_frame_without_padding () =
  let info = {Serialize.flags = 12; stream_id = 10; padding = None} in
  let f = Faraday.create 24 in
  Serialize.write_frame f info
    (Types.PushPromiseFrame (12, "this is dummy")) ;
  let output = Faraday.serialize_to_string f in
  Alcotest.(check string) "Serialized" (Util.string_of_hex wire') output

let tests =
  [ ("Can parse push promise frame", `Quick, parse_push_promise_frame)
  ; ( "Can serialize push promise frame with padding"
    , `Quick
    , serialize_push_promise_frame_with_padding )
  ; ( "Can serialize push promise frame without padding"
    , `Quick
    , serialize_push_promise_frame_without_padding ) ]
