let take_fifth xs =
  if List.length xs < 5 then 0
  else List.nth xs 4

let sort_desc xs = xs
  |> List.sort Pervasives.compare
  |> List.rev

let last xs = xs |> List.rev |> List.hd

let any_zeroes xs = xs |> List.exists @@ (=) 0
