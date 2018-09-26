type 'a tree = 
  | Leaf 
  | Node of 'a * 'a tree * 'a tree

let t1 =
  Node(4,
    Node(2,
      Node(1, Leaf, Leaf),
      Node(3, Leaf, Leaf)
    ),
    Node(5,
      Node(6, Leaf, Leaf),
      Node(7, Leaf, Leaf)
    )
  )

let t2 = 
  Node(4,
    Node(2,
      Node(1, Leaf, Leaf),
      Node(3, Leaf, Leaf)
    ),
    Node(5,
      Node(2,
        Node(1, Leaf, Leaf),
        Node(3, Leaf, Leaf)
      ),
      Node(7, Leaf, Leaf)
    )
  )

let rec size = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size l + size r

let rec depth = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (depth l) (depth r)

let rec shape t1 t2 =
  match (t1, t2) with
  | (Leaf, Leaf) -> true
  | (Leaf, _) | (_, Leaf) -> false
  | (Node (n1, l1, r1), Node (n2, l2, r2)) -> n1 = n2 && shape l1 l2 && shape r1 r2

let _ = shape t1 t2
