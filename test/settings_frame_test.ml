open Http2

let wire = "00000C040000000000000100002000000300001388"

let serialize_settings () =
  let input =
    [ (Types.SettingsHeaderTableSize, 8192)
    ; (Types.SettingsMaxConcurrentStreams, 5000) ]
  in
  let info = {Serialize.flags = 0; stream_id = 0; padding = None} in
  let f = Faraday.create 12 in
  Serialize.write_frame f info (Types.SettingsFrame input) ;
  let output = Faraday.serialize_to_string f in
  Alcotest.(check string)
    "Serialize settings frame" wire (Util.hex_of_string output)

let tests = [("Can serialize settings frame", `Quick, serialize_settings)]
