let rec product = function
  | [] -> 1
  | x::xs -> x * product xs

let rec concat = function
  | [] -> ""
  | x::xs -> x ^ concat xs

let rec take_helper n xs ys =
  match n, xs with
  | 0, _ | _, [] -> List.rev ys
  | _, x::xs -> take_helper (pred n) xs (x::ys)

let rec take n xs = take_helper n xs []

let rec drop n xs =
  match n, xs with
  | 0, _ | _, [] -> xs
  | _, _::xs -> drop (pred n) xs
