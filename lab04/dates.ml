let validate_year (year, _, _) = year > 0

let validate_month (_, month, _) = month > 0 && month < 13

let validate_day (_, month, day) =
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> day <= 31
  | 4 | 6 | 9 | 11 -> day <= 30
  | 2  -> day <= 28
  | _ -> false

let validate_date date =
  if validate_year date && validate_month date && validate_day date
  then date
  else failwith "Dates not valid"

let validate_dates dates = dates |> List.map validate_date

let is_before (year1, month1, day1) (year2, month2, day2) =
  let _ = validate_dates [(year1, month1, day1); (year2, month2, day2)] in
  year1 < year2 && month1 < month2 && day1 < day2

let rec earliest = function
  | [] -> None
  | [x] -> Some x
  | x::y::xs -> if is_before x y then earliest (x::xs)
                else earliest (y::xs)

let _ = earliest [(2013, 2, 1); (2014, 2, 1)]
