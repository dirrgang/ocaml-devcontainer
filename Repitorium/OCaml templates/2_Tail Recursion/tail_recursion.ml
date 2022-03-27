(* TAIL RECURSION *)
(* We again use our playing cards data type. *)
type playing_card =
  | Ace
  | King
  | Queen
  | Jack
  | Number of int

let value_of_card = function
  | Ace -> 11
  | King | Queen | Jack -> 10
  | Number x -> x

(* We now want to make the sum_values function tail recursive using a helper function with an extra parameter, the accumulator (acc) *)
let rec sum_values = function
  | [] -> 0
  | x::xs -> value_of_card x + sum_values xs
(* tail recursive: *)
let sum_values_tl l = 
  let rec aux acc = 
    function [] -> acc 
    | x :: xs -> aux (x + acc) xs in 
    aux 0 l

(* For each given function f, write a tail recursive version f_tl that implements the same functionality but only uses constant stack space. You may use the tail recursive functions you already implemented as helper functions. *)

(* 1 : faculty *)
let rec fac n =
  if n < 2 then 1 else n * fac (n-1)
(* tail recursive: *)
let fac_tl n = 
  let rec aux n acc = if n < 2 then 1 else fac (acc + acc * n-1) in
  aux n 1

(* 2 : replicate *)
let rec replicate x = function
  | 0 -> []
  | n -> x::replicate x (n-1)
(* tail recursive: *)
let replicate_tl x n = 
  let rec aux x acc = function
  | 0 -> acc
  | n -> aux x (x :: acc) (n - 1) in
  aux x [] n

(* 3 : reverse *)
let rec reverse = function
  | [] -> []
  | x::xs -> reverse xs @ [x]
(* tail recursive: *)
let reverse_tl l = 
  let rec aux acc = function
  [] -> acc
  | x :: xs -> aux (x :: acc) xs in
  aux [] l

(* 4 : map *)
let rec map f = function
  | [] -> []
  | x::xs -> f x :: map f xs
(* tail recursive: *)
let map_tl f l =
  let rec aux f acc = function
  | [] -> acc
  | x :: xs -> aux f (f x :: acc) xs in
  List.rev (aux f [] l)

(* 6 : append *)
let rec append xs ys =
  match xs with
    | [] -> ys
    | x::xs -> x :: append xs ys
(* tail recursive: *)
let append_tl l1 l2 =
  let rec aux acc l1 l2 = match (l1, l2) with
  | ([], []) -> acc
  | ([], ys) -> aux acc ys []
  | (x :: xs, _) -> aux (x :: acc) xs l2 in
  List.rev (aux [] l1 l2)

let l1 = [1; 2; 3; 4]
let l2 = [5; 6; 7; 8]