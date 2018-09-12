open Frames
open Parse

let parse_success wire =
  let req_payload = Hex.to_string (`Hex wire) in
  match Angstrom.parse_string parse_frame req_payload with
  | Ok frame -> frame
  | _ -> failwith "ERROR"

let string_of_hex s = Hex.to_string (`Hex s)

let hex_of_string s =
  let (`Hex hex) = Hex.of_string s in
  String.uppercase_ascii hex
