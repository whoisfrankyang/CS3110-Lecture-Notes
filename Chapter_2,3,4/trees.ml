type 'a tree =
  | Leaf 
  | Node of 'a * 'a tree * 'a tree 

let t = 
    Node (1,
          Node ( 2, Leaf, Leaf),
          Node (3, Leaf, Leaf))

let rec size = function 
  | Leaf -> 0
  | Node (_, l, r) -> 1+ size l + size r 

let rec sum = function 
  | Leaf -> 0
  | Node (v, l, r) -> v + sum l + sum r

let rec map f = function 
  | Leaf -> Leaf 
  | Node (v,l,r) -> Node (f v, map f l, map f r)

let add1 t = map succ t 

let rec fold f acc = function 
  | Leaf -> acc 
  | Node (v,l,r) -> f v (fold f acc l) (fold f acc r)

let sum t = fold (fun x y z -> x + y + z) 0 t 

