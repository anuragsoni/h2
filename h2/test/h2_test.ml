let test_suites : unit Alcotest.test list =
  [("Priority queue tests", Priority_test.tests)]

let () = Alcotest.run "H2 tests" test_suites
