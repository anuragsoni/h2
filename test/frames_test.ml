let () =
  Alcotest.run "Frames tests"
    [ ("Dataframe tests", Data_frame_test.tests)
    ; ("Priority frame tests", Priority_frame_test.tests)
    ; ("RSTStream frame tests", Rst_stream_frame_test.tests) ]
