open OUnit
open Library

let take_fifth_tests = "test suite for take_fifth" >::: [
  "empty list" >:: (fun _ -> assert_equal 0 (take_fifth []));
  "length < 5" >:: (fun _ -> assert_equal 0 (take_fifth [1;2;3]));
  "length = 5" >:: (fun _ -> assert_equal 5 (take_fifth [1;2;3;4;5]));
  "length > 5" >:: (fun _ -> assert_equal 5 (take_fifth [1;2;3;4;5;6;7]));
]

let sort_desc_tests = "test suite for sort_desc" >::: [
  "empty list"     >:: (fun _ -> assert_equal [] (sort_desc []));
  "non empty list" >:: (fun _ -> assert_equal [3;2;1] (sort_desc [1;2;3]));
]

let last_tests = "test suite for last" >::: [
  "non empty list" >:: (fun _ -> assert_equal 3 (last [1;2;3]));
]

let any_zeroes_tests = "test suite for any_zeroes" >::: [
  "without zero" >:: (fun _ -> assert_equal false (any_zeroes [1;2;3]));
  "with zero"    >:: (fun _ -> assert_equal true (any_zeroes [0;1;2]));
]

let tests = [
  take_fifth_tests;
  sort_desc_tests;
  last_tests;
  any_zeroes_tests
]

let _ = tests |> List.map run_test_tt_main
