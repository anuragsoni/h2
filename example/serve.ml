open Core
open Async

let http2_error_to_string = function
  | Frames.Types.ConnectionError (e, m) ->
      sprintf "Connection error: %d - %s" (Frames.Types.error_code_of_id e) m
  | Frames.Types.StreamError (e, s) ->
      sprintf "Stream error: %d, %d" (Frames.Types.error_code_of_id e) s

let run ~port =
  let host_and_port =
    Tcp.Server.create ~on_handler_error:`Raise (Tcp.Where_to_listen.of_port port)
      (fun _addr r w ->
        Pipe.transfer (Reader.pipe r) (Writer.pipe w) ~f:(fun payload ->
            Log.Global.printf "Payload: %s" payload ;
            if payload = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" then
              let f = Faraday.create 0x1000 in
              Frames.Serialize.write_frame f {Frames.Serialize.flags=0; stream_id=1; padding=None} (Frames.Types.SettingsFrame []);
              Faraday.serialize_to_string f
            else
              let parsed =
                Angstrom.parse_string
                  (Frames.Parse.parse_frame Frames.Types.default_settings)
                  payload
              in
              ( match parsed with
              | Ok parsed' -> (
                match parsed' with
                | Ok frame ->
                    Log.Global.printf "Parsing succeeded. StreamId: %d \n"
                      frame.frame_header.stream_id
                | Error e ->
                    Log.Global.printf "HTTP2 error %s\n"
                      (http2_error_to_string e) )
              | Error e -> Log.Global.printf "Parsing error: %s\n" e ) ;
              let f = Faraday.create 0x1000 in
              Frames.Serialize.write_frame f {Frames.Serialize.flags=0; stream_id=1; padding=None} (Frames.Types.SettingsFrame []);
              Faraday.serialize_to_string f) )
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t) ;
  Deferred.never ()

let () =
  Command.async_spec ~summary:"Sample server"
    Command.Spec.(
      empty
      +> flag "-port"
           (optional_with_default 8080 int)
           ~doc:" Port to listen on (default 8080)")
    (fun port () -> run ~port)
  |> Command.run
