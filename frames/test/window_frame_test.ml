open Frames

let wire = "000004080000000032000003E8"

let serialize_window_frame () =
  let info = {Serialize.flags = 0; stream_id = 50l; padding = None} in
  let f = Faraday.create 4 in
  Frames.Serialize.write_frame f info (Types.WindowUpdateFrame 1000) ;
  let output = Faraday.serialize_to_string f in
  Alcotest.(check string) "Serialize" wire (Util.hex_of_string output)

let tests = [("Serialize window frame", `Quick, serialize_window_frame)]
