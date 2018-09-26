type 'a tree = 
  | Leaf 
  | Node of 'a * 'a tree * 'a tree

let bst = 
  Node((2, "two"),
    Node((1, "one"), Leaf, Leaf),
    Node((3, "three"), Leaf, Leaf)
  )

let not_bst =
  Node((10, "two"),
    Node((11, "one"), Leaf, Leaf),
    Node((9, "three"), Leaf, Leaf)
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
