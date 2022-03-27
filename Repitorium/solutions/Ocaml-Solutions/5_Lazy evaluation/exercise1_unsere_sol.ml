(* Recall our sequence type *)
type 'a seq = Cons of 'a * (unit -> 'a seq)

(* Implement a function take : 'a seq -> int -> 'a list.
 * take xs n should return a list containing the first n values of xs.
 *)
let rec take l n =
  let rec aux acc (Cons (c, t)) n =
    match n with 0 -> acc | n -> aux (c :: acc) (t ()) (n - 1)
  in
  aux [] l n |> List.rev

(* Implement a function identity : unit -> int seq.
 * The nth element of identity () should be n.
 *)
let identity () =
  let rec aux n = Cons (n, fun () -> aux (n + 1)) in
  aux 0

(* Implement a function from_fun : (int -> 'a) -> 'a seq.
 * the nth element of from_fun f should be f n.
 *)
let from_fun f =
  let rec loop n = Cons (f n, fun () -> loop (n + 1)) in
  loop 0

(* Implement a function add : int seq -> int seq -> int seq
 * that adds two sequences componentwise.
 *)
let rec add (Cons (a1, t1)) (Cons (a2, t2)) =
  Cons (a1 + a2, fun () -> add (t1 ()) (t2 ()))

(* Let's define the 'thribonacci sequence' t_n as
 * t_0 := 0,
 * t_1 := 1,
 * t_2 := 2,
 * t_{n+3} := t_n + t_{n+1} + t_{n+2} for n >= 0.
 * Implement this sequence as thribonacci : int seq.
 *)
let thribonacci =
  let rec aux a b c = Cons (a + b + c, fun () -> aux b c (a + b + c)) in
  Cons (0, fun () -> Cons (1, fun () -> Cons (2, fun () -> aux 0 1 2)))

let thribonacci_2 =
  let rec aux a b c = Cons (a, fun () -> aux b c (a + b + c)) in
  aux 0 1 2
