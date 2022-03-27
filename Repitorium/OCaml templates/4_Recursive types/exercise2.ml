(* Exercise 2 - Recursion on more complicated datatypes
 * Sadly, the runtime of List.append is linear in the length of the first argument.
 * In this task, we will implement our own list type, which (at least) won't have
 * this problem.
 * The type is given by:
 *)
type 'a liste = Single of 'a list | Multi of 'a liste list
(* The idea is that, instead of appending two lists [x1; ...; xn] and [y1; ...; ym],
 * we construct a new list: [[x1; ...; xn], [y1; ...; ym]].
 * Without syntactic sugar:
 * app (Single [x1; ...; xn]) (Single [y1; ...; ym]) = Multi [Single [x1; ...; xn]; Single [y1; ...; ym]]
 * Your task is to implement the following functions.
 * Note: you will implement a function consolidate : 'a liste -> 'a list. Do not use
 * this function to implement any other function!
 *)

(* Implement a function cons : 'a -> 'a liste -> 'a liste, that has the same
 * effect as List.cons. Define the function s.t. it has constant runtime.
 *)
let rec cons x xs = failwith "TODO"

(* Implement a function app : 'a liste -> 'a liste -> 'a liste, that concatenates
 * two lists. It should have constant runtime.
 *)
let rec app xs ys = failwith "TODO"

(* Define a function is_empty : 'a liste -> bool, that returns whether the list is
 * empty.
 *)
let rec is_empty = failwith "TODO"

(* Define a function len : 'a liste -> int, that returns the length of the list. *)
let rec len = failwith "TODO"

(* Define a function hd_opt : 'a liste -> 'a option, that returns the first element x
 * of the list (if present) as Some x. Otherwise, it should return None.
 *)
let rec hd_opt = failwith "TODO"

(* Define a function create : 'a list -> 'a liste, that constructs a new list
 * from a classic list.
 *)
let create = failwith "TODO"

(* Define a function consolidate : 'a liste -> 'a list, that converts a liste
 * into the list it represents. This function may need more runtime.
 *)
let rec consolidate = failwith "TODO"
