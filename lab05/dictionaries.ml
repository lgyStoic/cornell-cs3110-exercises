type 'a tree = 
  | Leaf 
  | Node of 'a * 'a tree * 'a tree

(* bst:

    2
   / \
  1   3
*)

let bst = 
  Node((2, "two"),
    Node((1, "one"), Leaf, Leaf),
    Node((3, "three"), Leaf, Leaf)
  )

(* big_bst:

         4
      /     \
     2       6
    / \     / \
   1   3   5   7
*)

let big_bst = 
  Node((4, "four"),
    Node((2, "two"),
      Node((1, "one"), Leaf, Leaf),
      Node((3, "three"), Leaf, Leaf)
    ),
    Node((6, "six"),
      Node((5, "five"), Leaf, Leaf),
      Node((7, "seven"), Leaf, Leaf)
    )
  )

(* not_bst:

    10
   /  \
  11   9
*)

let not_bst =
  Node((10, "two"),
    Node((11, "one"), Leaf, Leaf),
    Node((9, "three"), Leaf, Leaf)
  )

(* big_not_bst:

          4
       /     \
      2       6
    /  \     / \
   10   3   5   7
*)

let big_not_bst = 
  Node((4, "four"),
    Node((2, "two"),
      Node((10, "one"), Leaf, Leaf),
      Node((3, "three"), Leaf, Leaf)
    ),
    Node((6, "six"),
      Node((5, "five"), Leaf, Leaf),
      Node((7, "seven"), Leaf, Leaf)
    )
  )

let rec lookup key = function
  | Leaf -> None
  | Node ((k, v), l, r) -> begin
      if key = k then Some v
      else if key < k then lookup key l
      else lookup key r
    end

let rec insert key value = function
  | Leaf -> Node ((key, value), Leaf, Leaf)
  | Node ((k, v), l, r) ->
      if key < k then
        Node ((k, v), insert key value l, r)
      else if key > k then
        Node ((k, v), l, insert key value r)
      else
        Node ((k, value), l, r)

let rec is_bst node =
  match node with
    | Leaf -> true
    | Node ((key, _), left, right) ->
      match (left, right) with
        | (Leaf, Leaf) -> true
        | (Node ((k, _), _, _), Leaf) -> k < key && is_bst left
        | (Leaf, Node ((k, _), _, _)) -> k > key && is_bst right
        | (Node ((k1, _), l1, r1), Node ((k2, _), l2, r2)) ->
          k1 < key && key < k2 && is_bst left && is_bst right
