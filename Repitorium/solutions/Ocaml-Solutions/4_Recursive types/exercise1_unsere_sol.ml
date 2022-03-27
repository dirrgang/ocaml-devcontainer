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
  | Empty
  | Leaf of 'a
  | Node2 of 'a tree23 * 'a tree23 * 'a
  | Node3 of 'a tree23 * 'a tree23 * 'a tree23 * 'a * 'a

(* Define a function search : 'a -> 'a tree23 -> bool, that searches the tree for a given
 * element. It should return true iff the element was found.
 * Hint: use the built-in function compare.
 *)
let rec search x t =
  match t with
  | Empty -> false
  | Leaf y -> x = y
  | Node2 (l, r, a) -> (
      match compare x a with
      | 0 -> true
      | n when n > 0 -> search x r
      | _ -> search x l)
  | Node3 (l, m, r, a, b) -> (
      match compare x a with
      | 0 -> true
      | n when n < 0 -> search x l
      | _ -> (
          match compare x b with
          | 0 -> true
          | n when n < 0 -> search x m
          | _ -> search x r))

let x = Leaf (Some 3)

let y = Leaf (fun () -> Node2 (Leaf None, Leaf None, Some 3))
