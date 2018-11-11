(* Exercise: mystery operator 1.
 * Application operator. Right-associative
 *
 * square 2 + 2 = (square 2) + 2 = 4 + 2 = 6
 * square $ 2 + 2 = square $ (2 + 2) = square $ 4 = 16
 *)
let ($) f x = f x

(* Function composition.
 * With awful parentheses.
 *
 * compose square square 10 = square (square 10) = square 100 = 10000
 *)
let compose f g x = f (g x)

(* Exercise: mystery operator 2.
 * Function composition with pipe.
 * Without parentheses.
 *
 * (square @@ square) 10 = 10 |> square |> square = 10000
 * square @@ square $ 10 = (square @@ square) $ 10  = 10000
 *)
let (@@) f g x = x |> g |> f

(* Function composition with application operator.
 * Without parentheses.
 *
 * compose' square square 10 = square $ (square 10) = square $ 100 = 10000
 *)
let compose' f g x = f $ g x

(* Exercise: repeat.
 * Applies `f` to `x` a total of `n` times.
 *
 * repeat square 2 10 = square (square 10) = square 100 = 10000
 *)
let rec repeat f n x =
  if n = 0 then x else repeat f (n - 1) (f x)

(* Exercise: product.
 * Product of a list of floats
 * Tail-recursive. Point-free. Beautiful.
 *)
let product_left = List.fold_left ( *. ) 1.0

(* Exercise: product.
 * Product of a list of floats
 * Not tail-recursive. Not point-free. Awful.
 *)
let product_right xs = List.fold_right ( *. ) xs 1.0

(* Tail-recursively computes the list containing
 * all the integers from `i` to `j` inclusive.
 *)
let (--) i j =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
    in from i j []

let odd x = x mod 2 = 0

let cube x = x * x * x

(* Exercise: sum_cube_odd pipeline.
 * Sum of the cubes of all the odd numbers
 * between `0` and `n` inclusive.
 *)
let sum_cube_odd n =
  0 -- n
  |> List.filter odd
  |> List.map cube
  |> List.fold_left (+) 0

(* Exercise: exists.
 * `exists_*` returns whether at least one element
 * of the list satisfies the predicate `p`.
 *)
let rec exists_rec p = function
| [] -> false
| h::t -> p h || exists_rec p t

let exists_fold p = List.fold_left (fun acc x -> acc || p x) false

let exists_lib = List.exists

(* Exercise: library uncurried.
 * Write uncurried versions of these library functions.
 *)
let uncurried_append (xs, ys) = List.append xs ys
let uncurried_compare (x, y) = Char.compare x y
let uncurried_max (x, y) = max x y

(* Exercise: uncurry.
 * Write a function `uncurry` that
 * takes in a curried function and returns
 * the uncurried version of that function.
 *)
let uncurry f (x, y) = f x y

(* Exercise: curry.
 * Write the inverse (regarding `uncurry`) function `curry`.
 *)
let curry f x y = f (x, y)

(* Exercise: map composition.
 * Replace `List.map f (List.map g lst)`
 * with an equivalent expression
 * that calls List.map only once.
 *)
let map_composition f g = List.map (compose f g)

(* Exercise: more list fun.
 * Find those elements of a list of strings
 * whose length is strictly greater than 3.
 *)
let greater_than_3 = List.filter (fun x -> String.length x > 3)

(* Exercise: more list fun.
 * Add 1.0 to every element of a list of floats.
 *)
let add1 = List.map ((+.) 1.0)

(* Exercise: more list fun.
 * Given a list of strings `strs` and another string `sep`,
 * produce the string that contains every element
 * of `strs` separated by `sep`.
 *)
let join xs sep =
  match xs with
  | []    -> ""
  | x::xs -> List.fold_left (fun acc x -> acc ^ sep ^ x) x xs


(* Exercise: tree map.
 * Write a function that applies
 * a function to every node of a tree.
 *)

type 'a tree =
| Leaf
| Node of 'a * 'a tree * 'a tree

let rec map_tree f = function
| Leaf -> Leaf
| Node (n, l, r) -> Node (f n, map_tree f l, map_tree f r)

let add1_tree = map_tree succ

(* Exercise: association list keys.
 * Write a function that returns a list
 * of the unique keys in an association list.
 *)
let keys xs = xs |> List.map fst |> List.sort_uniq compare
