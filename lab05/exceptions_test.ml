open OUnit
open Exceptions

let test = "suite for list_max" >::: [
  "empty"    >:: (fun _ -> assert_raises (Failure "empty") (fun () -> list_max []));
  "nonempty" >:: (fun _ -> assert_equal  2                 (list_max [1;2]));
]

let _ = run_test_tt_main test
