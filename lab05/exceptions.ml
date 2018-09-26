let rec list_max = function
  | [] -> failwith "empty"
  | [x] -> x
  | x::xs -> max x (list_max xs)

let rec list_max_string = function
  | [] -> "empty"
  | [x] -> string_of_int x
  | x::xs -> string_of_int @@ max x (list_max xs)
