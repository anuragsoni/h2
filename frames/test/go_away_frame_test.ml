open Frames

let wire = "0000170700000000000000001E00000009687061636B2069732062726F6B656E"

let serialize_go_away_frame () =
  let info = {Serialize.flags = 0; stream_id = 0; padding = None} in
  let f = Faraday.create 23 in
  Frames.Serialize.write_frame f info
    (Types.GoAwayFrame (30, Types.error_code_to_id 9, "hpack is broken")) ;
  let output = Faraday.serialize_to_string f in
  Alcotest.(check string) "Serialize" wire (Util.hex_of_string output)

let tests = [("Serialize go away frame", `Quick, serialize_go_away_frame)]
