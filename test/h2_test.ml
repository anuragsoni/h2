let frame_tests =
  [ ("Dataframe tests", Data_frame_test.tests)
  ; ("Headers frame tests", Headers_frame_test.tests)
  ; ("Priority frame tests", Priority_frame_test.tests)
  ; ("RSTStream frame tests", Rst_stream_frame_test.tests)
  ; ("Settings frame tests", Settings_frame_test.tests)
  ; ("Push promise frame tests", Push_promise_frame_test.tests)
  ; ("Ping frame test", Ping_frame_test.tests)
  ; ("GoAway frame test", Go_away_frame_test.tests)
  ; ("Window frame test", Window_frame_test.tests)
  ; ("Continuation frame test", Continuation_frame_test.tests) ]

let priority_queue_tests = [("Priority queue tests", Priority_test.tests)]

let () =
  Alcotest.run "H2 tests" (List.concat [frame_tests; priority_queue_tests])
