let first_is_bigred = function
  | [] -> false
  | x::_ -> x = "bigred"

let has_two_or_four = function
  | [_; _] | [_; _; _; _] -> true
  | _ -> false

let first_two_equal = function
  | x::y::_ -> x = y
  | _ -> false
