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
let cons x xs =
  match xs with
  | Single l -> Single (x :: l)
  | Multi l -> Multi (Single [ x ] :: l)

let cons2 x xs = Multi [ Single [ x ]; xs ]

let rec cons_rec x xs =
  match xs with
  | Single l -> Single (x :: l)
  | Multi [] -> Single [ x ]
  | Multi (l :: ls) -> Multi (cons_rec x l :: ls)

(* Implement a function app : 'a liste -> 'a liste -> 'a liste, that concatenates
 * two lists. It should have constant runtime.
 *)
let app xs ys = Multi [ xs; ys ]

(* nur bis hier erstmal :) *)

(* Define a function is_empty : 'a liste -> bool, that returns whether the list is
 * empty.
 *)
let rec is_empty l =
  match l with Single l -> l = [] | Multi l -> List.for_all is_empty l

(* Define a function len : 'a liste -> int, that returns the length of the list. *)
let rec len = function
  | Single l -> List.length l
  | Multi el -> List.fold_left (fun acc x -> len x + acc) 0 el

let _ = Single [ 1; 2; 3 ]

(* Define a function hd_opt : 'a liste -> 'a option, that returns the first element x
 * of the list (if present) as Some x. Otherwise, it should return None.
 *)
let rec hd_opt = function
  | Single l -> List.find_opt (fun _ -> true) l
  | Multi el ->
      List.fold_left (fun acc x -> if acc = None then hd_opt x else acc) None el

let _ =
  Multi [ Multi [ Single []; Single []; Single [] ]; Single []; Single [ 3 ] ]

(* Define a function create : 'a list -> 'a liste, that constructs a new list
 * from a classic list.
 *)
let create l = Single l

(* Define a function consolidate : 'a liste -> 'a list, that converts a liste
 * into the list it represents. This function may need more runtime.
 *)
let rec consolidate = function
  | Single l -> l
  | Multi l -> List.fold_left (fun acc x -> acc @ consolidate x) [] l
