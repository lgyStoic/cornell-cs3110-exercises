let safe_hd = function
  | [] -> None
  | x::_ -> Some x

let safe_tl = function
  | [] -> None
  | _::xs -> Some xs
