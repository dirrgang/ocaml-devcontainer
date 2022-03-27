(* Sometimes, we want to write an expression, but don't want to evaluate it yet,
 * but at a later time.
 * One use-case scenario would be if we have an expression that is expensive
 * to evaluate, and we don't know yet if we even need the value.
 * Another typical example are expressions that can not be fully evaluated, e.g.
 * sequences.
 * The concept of only evaluating an expression if its value is actually needed
 * is referred to as _lazy evaluation_.
 *)

(* A simple way to implement lazy evaluation is to hide expressions behind a
 * (nullary) function (this function is then called a "thunk"):
 *)
let lazy_expression () = print_endline "evaluation :)"

(* This definition does not print anything: the expression 'print_endline "..."'
 * is not evaluated.
 * If we want to actually evaluate it, we simply apply the function to the unit
 * tuple, forcing evaluation:
 *)
let _ = lazy_expression ()

(* Thunks will come in handy when dealing with impure functions later.
 * For now, let's take a look at how to implement infinite sequences with them.
 * The construction is similar to the construction of lists, but we don't have
 * a nil type (since we only consider infinite sequences).
 *)
type 'a seq = Cons of 'a * (unit -> 'a seq)

(* Notice how the tail is hidden in a thunk - otherwise, we could not represent any
 * infinite sequence!
 * Now, let's look at two examples on how to use this type.
 * First, define a function that creates a constant sequence:
 *)
let rec constant_seq c = Cons (c, fun () -> constant_seq c)

(* Again, notice how we hide the tail in a thunk, so we don't actually have to evaluate
 * it.
 * Now, let's define a function that returns the nth sequence element, similar to
 * List.nth.
 *)
let rec get_nth (Cons (c, t)) n = if n = 0 then c else get_nth (t ()) (n - 1)

(* Note: there is also a built-in type for lazy evaluation (not covered in the lecture).
 * It can be constructed as follows:
 *)
let lazy_expression_2 = lazy (print_endline "evaluation (:")

(* For evaluation, there is the Lazy.force function: *)
let _ = Lazy.force lazy_expression_2

(* However, there is a difference to thunks: the built-in lazy type supports
 * memoization, meaning that the value is cached. This can be useful for expensive
 * evaluations, but changes behavior when considering functions with side-effects.
 * for example, since we already forced evaluation of lazy_expression_2, the following
 * will produce no output.
 *)
let _ = Lazy.force lazy_expression_2

(* Meanwhile, evaluating our thunk again does indeed evaluate the print expression
 * another time:
 *)
let _ = lazy_expression ()
