open OUnit
open Lab06

let odd x = x mod 2 = 0

let test = "suite for lab06" >::: [
  "exists_rec: true"  >:: (fun _ -> assert_equal true  (exists_rec odd [1;2;3]));
  "exists_rec: false" >:: (fun _ -> assert_equal false (exists_rec odd [1;3;5]));
  "exists_rec: empty" >:: (fun _ -> assert_equal false (exists_rec odd []));

  "exists_fold: true"  >:: (fun _ -> assert_equal true  (exists_fold odd [1;2;3]));
  "exists_fold: false" >:: (fun _ -> assert_equal false (exists_fold odd [1;3;5]));
  "exists_fold: empty" >:: (fun _ -> assert_equal false (exists_fold odd []));

  "exists_lib: true"  >:: (fun _ -> assert_equal true  (exists_lib odd [1;2;3]));
  "exists_lib: false" >:: (fun _ -> assert_equal false (exists_lib odd [1;3;5]));
  "exists_lib: empty" >:: (fun _ -> assert_equal false (exists_lib odd []));
]

let _ = run_test_tt_main test
