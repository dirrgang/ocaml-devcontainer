(* Exercise 1 - Recursion on trees
 * In this task, we want to define a search tree, where each node should have 2 or 3 children.
 * Define a type 'a tree23, where
 * - each node should have 2 or 3 children, and contain one element of type 'a less than the
      number of children.
   - each leaf should contain one element of type 'a. 
   Also, it should be possible to represent an empty tree with your type.

   The idea is that, if e.g. a node has three children l, m, r and two elements a, b, it
   should hold that, informally, "l < a < m < b < r", or more formally: all elements in l
   need to be smaller than a, all elements in m must be greater than a but less than b, and
   all elements in r must be greater than b. In particular, no duplicates should be contained.
   Your implementation must satisfy this invariant!
 *)
type 'a tree23 = 
  Leaf of 'a 
  | Node3 of 'a tree23 * 'a * 'a tree23 * 'a  *'a tree23 
  | Node2 of 'a tree23 * 'a * 'a tree23



(* Define a function search : 'a -> 'a tree23 -> bool, that searches the tree for a given
 * element. It should return true iff the element was found.
 * Hint: use the built-in function compare.
 *)

 
let rec search (x: 'a) (t: 'a tree23) = match t with
| Leaf lf -> x = lf
| Node3 (l, a, m, b, r) -> (match (compare a x < 0, compare b x > 0) with 
| (true, _ ) -> search x l
| (_, true) -> search x r
| (_, _) -> if x = a || x = b then true else search x m)
| Node2 (l, m, r) -> (match compare m x < 0 with 
  | true -> search x l
  | false -> if x = m then true else search x r)