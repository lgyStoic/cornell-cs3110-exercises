let inc x = x + 1

let rec fact n =
  if n = 0
  then 1
  else n * fact (n - 1)

let rec fib n =
  if n = 1 or n = 2
  then 1
  else fib (n - 1) + fib (n - 2)

let rec fib_tail_helper n x y =
  if n = 1
  then y
  else fib_tail_helper (n - 1) y (x + y)

let fib_tail n = fib_tail_helper n 0 1

let id x = x

let devide ~numerator ~denominator = numerator /. denominator

let ($$) x y = (x +. y) /. 2.
