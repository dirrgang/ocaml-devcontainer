(* Find the errors and informally describe what the functions (should) do *)

(*1*)
let l1 = [1;2;3;4;5]

(*2*)
let rec f2 l = 
  match l with
    [] -> 0
    |x::xs -> 1 + f2 xs

(*3*)
let rec f3 = function
		| [] -> []
		| x::xs -> x :: x :: f3 xs

(*4*)
let rec f4 v l =
  let rec loop c = function 
		| [] -> 0
		| x::xs -> if x > v then loop (c+1) xs else loop c xs
in loop 0 l

(*5*)
let f5 n =
  let rec impl : int -> int list = function k ->
    if k == n then [k] else k :: impl (k+1)
in impl 0

(*6*)
let f6 a b c = (a +. b) *. c

(*7*)
let rec f7 = function
  | [] -> [0]
  | x::xs -> [2*x]@f7 xs