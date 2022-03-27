(* Reminder: List recursion
 * Lists are a datatype. This datatype could be defined as follows:
 *)

type 'a my_list = Nil | Cons of 'a * 'a my_list

(* "[]" and "x::xs" can then be interpreted as syntactic sugar for "nil" and
 * "Cons (x, xs)".
 * Functions on lists can be defined recursively via match-expressions:
 *)

let rec length l = match l with Nil -> 0 | Cons (x, xs) -> 1 + length xs

(* In the same way, we can define other recursive datatypes.
 * A classic example: Trees.
 *)

(* Classic binary tree *)
type tree = Leaf | Node of tree * tree

(* If we now want to define a function on such a recursive datatype, we can proceed
 * similarly as in the case of lists. We match our datatype, and can now
 * recursively call our function on every occurrence of the same datatype.
 *)
let rec tree_size t =
  match t with Leaf -> 0 | Node (l, r) -> 1 + tree_size l + tree_size r

(* Classic tree *)
type mtree = MNode of mtree list

let rec mtree_size t =
  match t with MNode l -> List.fold_left ( + ) 1 (List.map mtree_size l)

(* Recursive datatypes can also be polymorphic *)
type 'a mytype =
  | Tag of 'a
  | Pair of 'a mytype * 'a mytype
  | Assoc of 'a * 'a mytype

let rec collect = function
  | Tag x -> [ x ]
  | Pair (x, y) -> collect x @ collect y
  | Assoc (x, y) -> x :: collect y

(* Even records can be recursive datatypes *)
type 'a mr = { x : int; y : ('a, 'a mr) Either.t }

let rec f { x; y } =
  match y with Either.Left a -> x | Either.Right a -> x + f a