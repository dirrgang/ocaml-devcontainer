(* In this exercise, we want to implement intervals.
 * In order to do this, we will first define the specification as module type.
 *)

module type Interval = sig
  type t (* internal representation of the interval *)

  type endpoint (* type of the points contained, e.g. int or float *)

  (* functions we want to provide. *)
  val create : endpoint -> endpoint -> t

  val is_empty : t -> bool

  val contains : t -> endpoint -> bool
  (* contains i x should check whether x is in i *)

  val intersect : t -> t -> t
end

(* First, define a module IntInterval, implementing Interval *)
(* TODO: define a module *)

(* Now, we want to generalize this implementation. Instead of implementing
 * Interval for ints, we want to implement it for any ordered type.
 * For this, we will first have to define what an ordered type is:
 *)
module type Ord = sig
  type t (* the actual type *)

  type cmp_type = Less | Eq | Greater

  val cmp : t -> t -> cmp_type
end

(* Now, define a functor OrdInterval implementing Interval, given a module of type Ord,
 * i.e. define a functor with functor type "functor (X : Ord) -> Interval".
 *)
(* TODO: define functor *)

(* Now, implement the Ord type for integers in a module IntOrd. *)
(* TODO: define module *)

(* Now use the functor to obtain a module of type Interval for integers. *)
(* TODO: define another IntInterval module via functor *)

(* Similarly, implement the Ord type for rationals and then use it to obtain
 * a module for rational intervals.
 * Hint: Rationals are fractions p/q, where p and q are ints. How can you
 * represent such a fraction as a type?
 *)
(* TODO: implement Rational module of module type Ord *)

(* TODO: define module for rational intervals *)

(* If we have a module X of type Ord, we can compare two values of type X.t.
 * This gives us a canonical way of defining a minimum function, however,
 * we can not define functions in module types.
 * The solution is to define a functor, mapping X to an extension of Ord.
 * First, define an extension of the Ord module type by a minimum and a
 * maximum function.
 *)
(* TODO: define OrdExtended *)

(* Now, implement a functor, mapping modules of type Ord to OrdExtended. *)
(* TODO: implement functor *)