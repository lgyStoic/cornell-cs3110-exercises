open OUnit
open Add

let tests = "test suite for add" >::: [
  "1 2" >:: (fun _ -> assert_equal 3 (add 1 2));
  "7 2" >:: (fun _ -> assert_equal 9 (add 7 2));
]

let _ = run_test_tt_main tests
