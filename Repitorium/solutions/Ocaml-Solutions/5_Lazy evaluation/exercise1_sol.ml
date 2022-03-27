(* Recall our sequence type *)
type 'a seq = Cons of 'a * (unit -> 'a seq)

(* Implement a function take : 'a seq -> int -> 'a list.
 * take xs n should return a list containing the first n values of xs.
 *)
let rec take (Cons (c, t)) n = if n = 0 then [] else c :: take (t ()) (n - 1)

(* Implement a function identity : unit -> int seq.
 * The nth element of identity () should be n.
 *)
let identity =
  let rec id_aux m = Cons (m, fun () -> id_aux (m + 1)) in
  fun () -> id_aux 0

(* Implement a function from_fun : (int -> 'a) -> 'a seq.
 * the nth element of from_fun f should be f n.
 *)
let from_fun f =
  let rec aux m = Cons (f m, fun () -> aux (m + 1)) in
  aux 0

(* Implement a function add : int seq -> int seq -> int seq
 * that adds two sequences componentwise.
 *)
 let rec add (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (h1 + h2, fun () -> add (t1 ()) (t2 ()))

(* Let's define the 'thribonacci sequence' t_n as
 * t_0 := 0,
 * t_1 := 1,
 * t_2 := 2,
 * t_{n+3} := t_n + t_{n+1} + t_{n+2} for n >= 0.
 * Implement this sequence as thribonacci : int sequence.
 *)
 let thribonacci =
  let rec aux a b c = Cons (a, fun () -> aux b c (a+b+c)) in
  aux 0 1 2