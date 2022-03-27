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
  let rec loop acc = function
  | [] -> acc
  | x::xs -> loop (acc + value_of_card x) xs
in loop 0 l

(* For each given function f, write a tail recursive version f_tl that implements the same functionality but only uses constant stack space. You may use the tail recursive functions you already implemented as helper functions. *)

(* 1 : faculty *)
let rec fac n =
  if n < 2 then 1 else n * fac (n-1)
(* tail recursive: *)
let fac_tl k = 
  let rec loop acc n =
  if n < 2 then acc else loop (acc*n) (n-1) 
  in loop 1 k

let fac_tl_match k =
  let rec loop acc n =
    match n with 
      | i when i<2 -> acc
      | i -> loop (acc * i) (i-1)
  in loop 1 k

(* 2 : replicate *)
let rec replicate x = function
  | 0 -> []
  | n -> x::replicate x (n-1)
(* tail recursive: *)
let replicate_tl x n = 
  let rec aux x n acc = match n with
  | 0 -> acc
  | n -> aux x (n-1) (x::acc)
  in aux x n []

let replicate_tl x n =
  let rec loop acc y = function 0 -> acc | n -> loop (y :: acc) y (n - 1) in
loop [] x n

(* 3 : reverse *)
let rec reverse = function
  | [] -> []
  | x::xs -> reverse xs @ [x]
(* tail recursive: *)
let reverse_tl l =
  let rec loop acc = function
  | [] -> acc
  | x::xs -> loop (x::acc) xs
  in loop [] l

(* 4 : map *)
let rec map f = function
  | [] -> []
  | x::xs -> f x :: map f xs
(* tail recursive: *)
let map_tl f l =
  let rec map_iter acc = function
    | [] -> acc
    | x::xs -> map_iter ((f x)::acc) xs
in reverse_tl (map_iter [] l)

(* 6 : append *)
let rec append xs ys =
  match xs with
    | [] -> ys
    | x::xs -> x :: append xs ys
(* tail recursive: *)
let append_tl l1 l2 =
  let rec app_tail l1 acc =
    match l1 with
      | [] -> acc
      | x::xs -> app_tail xs (x::acc)
in app_tail (reverse_tl l1) l2

let f x = (x*x) + 2 * ((x*x)+1)

let f x =
  let sq n = n*n in 
  let b = (sq x)+1 in
  (sq x) + 2*b 

