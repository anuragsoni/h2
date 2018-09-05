open Http2

let wire_no_priority = "00000D010400000001746869732069732064756D6D79"

let wire_priority =
  "000022012C000000030F8000001409746869732069732064756D6D79546869732069732070616464696E67"

let serialize_headers_frame_no_priority () =
  let info = {Serialize.flags = 4; stream_id = 1; padding = None} in
  let f = Faraday.create 13 in
  Serialize.write_frame f info
    (Types.HeadersFrame (None, "this is dummy")) ;
  let output = Faraday.serialize_to_string f in
  Alcotest.(check string)
    "Serialize" wire_no_priority (Util.hex_of_string output)

let serialize_headers_frame_with_priority () =
  let info =
    {Serialize.flags = 44; stream_id = 3; padding = Some "This is padding"}
  in
  let priority = {Types.exclusive = true; stream_dependency = 20; weight = 9} in
  let f = Faraday.create 35 in
  Serialize.write_frame f info
    (Types.HeadersFrame (Some priority, "this is dummy")) ;
  let output = Faraday.serialize_to_string f in
  Alcotest.(check string) "Serialize" wire_priority (Util.hex_of_string output)

let tests =
  [ ( "Serialize headers with no priority"
    , `Quick
    , serialize_headers_frame_no_priority )
  ; ( "Serialize headers with priority"
    , `Quick
    , serialize_headers_frame_with_priority ) ]
