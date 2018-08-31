open Frames
open Types
open Parse

let parse_success wire =
  let req_payload = Hex.to_string (`Hex wire) in
  match Angstrom.parse_string (parse_frame default_settings) req_payload with
  | Ok parsed -> (
    match parsed with Ok frame -> frame | _ -> failwith "ERROR" )
  | _ -> failwith "ERROR"

let string_of_hex s =
  Hex.to_string (`Hex s)
