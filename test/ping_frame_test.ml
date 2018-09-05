open Http2

let wire = "0000080600000000006465616462656566"

let serialize_ping_frame () =
  let f = Faraday.create 8 in
  let info = {Serialize.flags = 0; stream_id = 0; padding = None} in
  Serialize.write_frame f info (Types.PingFrame "deadbeef") ;
  let output = Faraday.serialize_to_string f in
  Alcotest.(check string) "serialize" wire (Util.hex_of_string output)

let tests = [("Serialize ping frame", `Quick, serialize_ping_frame)]
