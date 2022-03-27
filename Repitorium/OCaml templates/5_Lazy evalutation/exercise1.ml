(* Recall our sequence type *)
type 'a seq = Cons of 'a * (unit -> 'a seq)

(* Implement a function take : 'a seq -> int -> 'a list.
 * take xs n should return a list containing the first n values of xs.
 *)
let rec take (Cons (c, t)) n = failwith "TODO"

(* Implement a function identity : unit -> int seq.
 * The nth element of identity () should be n.
 *)
let identity = failwith "TODO"

(* Implement a function from_fun : (int -> 'a) -> 'a seq.
 * the nth element of from_fun f should be f n.
 *)
let from_fun f = failwith "TODO"

(* Implement a function add : int seq -> int seq -> int seq
 * that adds two sequences componentwise.
 *)
let add = failwith "TODO"

(* Let's define the 'thribonacci sequence' t_n as
 * t_0 := 0,
 * t_1 := 1,
 * t_2 := 2,
 * t_{n+3} := t_n + t_{n+1} + t_{n+2} for n >= 0.
 * Implement this sequence as thribonacci : int sequence.
 *)
 let thribonacci = failwith "TODO"