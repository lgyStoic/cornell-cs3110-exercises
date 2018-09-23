open OUnit
open Lab03

(* returns:  [from i j l] is the list containing the integers from
 *   [i] to [j], inclusive, followed by the list [l].
 * example:  [from 1 3 [0] = [1;2;3;0]] *)
let rec from i j l =
  if i > j then l
  else from i (j-1) (j::l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
 *) 
let (--) i j =
  from i j []

let list = 1 -- 3
let longlist = 1 -- 1_000_000
let takelist = 1 -- 500_000
let droplist = 500_001 -- 1_000_000

let tests = "test suite for lab03" >::: [
  "product empty"   >:: (fun _ -> assert_equal 1 (product []));
  "product"         >:: (fun _ -> assert_equal 6 (product list));

  "concat empty"    >:: (fun _ -> assert_equal "" (concat []));
  "concat"          >:: (fun _ -> assert_equal "abc" (concat ["a"; "b"; "c"]));

  "take from empty" >:: (fun _ -> assert_equal [] (take 10 []));
  "take zero"       >:: (fun _ -> assert_equal [] (take 0 list));
  "take"            >:: (fun _ -> assert_equal takelist (take 500_000 longlist));

  "drop from empty" >:: (fun _ -> assert_equal [] (drop 10 []));
  "drop zero"       >:: (fun _ -> assert_equal list (drop 0 list));
  "drop"            >:: (fun _ -> assert_equal droplist (drop 500_000 longlist));
]

let _ = run_test_tt_main tests
