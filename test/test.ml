open OUnit2

let _ = run_test_tt_main ("SettlersOfOcaml" >::: [ SetupGameTests.tests; PlayGameTests.tests; ])
