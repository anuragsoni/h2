let () =
  Alcotest.run "Frames tests"
    [ ("Dataframe tests", Data_frame_test.tests)
    ; ("Priority frame tests", Priority_frame_test.tests)
    ; ("RSTStream frame tests", Rst_stream_frame_test.tests)
    ; ("Settings frame tests", Settings_frame_test.tests)
    ; ("Push promise frame tests", Push_promise_frame_test.tests)
    ; ("Ping frame test", Ping_frame_test.tests)
    ; ("GoAway frame test", Go_away_frame_test.tests)
    ; ("Window frame test", Window_frame_test.tests)
    ; ("Continuation frame test", Continuation_frame_test.tests) ]
