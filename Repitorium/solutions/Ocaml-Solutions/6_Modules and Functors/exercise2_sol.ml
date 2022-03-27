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
module IntInterval : Interval with type endpoint = int = struct
  type t = int * int

  type endpoint = int

  let create a b = (a, b)

  let is_empty (a, b) = a >= b

  let contains (a, b) c = c >= a && c <= b

  let intersect (a, b) (c, d) = (max a c, min b d)
end

(* Now, we want to generalize this implementation. Instead of implementing
 * Interval for ints, we want to implement it for any ordered type.
 * For this, we will first have to define what an ordered type is:
 *)
module type Ord = sig
  type t (* the actual type *)

  type cmp_type = Less | Eq | Greater

  val cmp : t -> t -> cmp_type
end

(* Now, define a functor OrdInterval implementing Interval, given a module of type Ord. *)
module OrdInterval (X : Ord) : Interval with type endpoint = X.t = struct
  type t = X.t * X.t

  type endpoint = X.t

  let create a b = (a, b)

  let is_empty (a, b) =
    match X.cmp a b with X.Greater | X.Eq -> false | X.Less -> true

  let contains (a, b) c =
    let v, w = (X.cmp a c, X.cmp c b) in
    (v = X.Less || v = X.Eq) && (w = X.Less || w = X.Eq)

  let intersect (a, b) (c, d) =
    let l = if X.cmp a c = X.Less then c else a in
    let u = if X.cmp b d = X.Less then b else d in
    (l, u)
end

(* Now, implement the Ord type for integers *)
module IntOrd : Ord = struct
  type t = int

  type cmp_type = Less | Eq | Greater

  let cmp a b = if a < b then Less else if a = b then Eq else Greater
end

(* Now use the functor to obtain a module of type Interval for integers. *)
module IntInterval2 = OrdInterval (IntOrd)

(* Similarly, implement the Ord type for rationals and then use it to obtain
 * a module for rational intervals.
 *)
module Rational : Ord = struct
  type t = int * int (* idea: represent p/q as (p, q) *)

  type cmp_type = Less | Eq | Greater

  (* Note: if q > 0 and t > 0, then p/q > s/t iff p*t > q*s *)
  let rec cmp (p, q) (s, t) =
    if q < 0 then cmp (-p, -q) (s, t)
    else if t < 0 then cmp (p, q) (-s, -t)
    else
      match (p * t) - (q * s) with
      | z when z > 0 -> Greater
      | z when z = 0 -> Eq
      | _ -> Less
end

module RationalInterval = OrdInterval (Rational)

(* If we have a module X of type Ord, we can compare two values of type X.t.
 * This gives us a canonical way of defining a minimum function, however,
 * we can not define functions in module types.
 * The solution is to define a functor, mapping X to an extension of Ord.
 * First, define an extension of the Ord module type by a minimum and a
 * maximum function.
 *)
module type OrdExtended = sig
  include Ord

  val minimum : t -> t -> t

  val maximum : t -> t -> t
end

(* Now, implement a functor, mapping modules of type Ord to OrdExtended. *)
module OrdToOrdExtended (X : Ord) : OrdExtended = struct
  include X

  let minimum a b = if cmp a b = Less then a else b

  let maximum a b = if cmp a b = Less then b else a
end
