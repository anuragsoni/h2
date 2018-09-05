open Http2

let wire = "000000090000000032"

let wire' = "00000D090000000032746869732069732064756D6D79"

let serialize_continuation_frame () =
  let info = {Serialize.flags = 0; stream_id = 50; padding = None} in
  let f = Faraday.create 0 in
  Serialize.write_frame f info (Types.ContinuationFrame "") ;
  let output = Faraday.serialize_to_string f in
  Alcotest.(check string) "serialize" wire (Util.hex_of_string output)

let serialize_continuation_frame' () =
  let info = {Serialize.flags = 0; stream_id = 50; padding = None} in
  let f = Faraday.create 13 in
  Serialize.write_frame f info (Types.ContinuationFrame "this is dummy") ;
  let output = Faraday.serialize_to_string f in
  Alcotest.(check string) "serialize" wire' (Util.hex_of_string output)

let tests =
  [ ( "Serialize continuation frame without header block"
    , `Quick
    , serialize_continuation_frame )
  ; ( "Serialize continuation frame with header block"
    , `Quick
    , serialize_continuation_frame' ) ]
