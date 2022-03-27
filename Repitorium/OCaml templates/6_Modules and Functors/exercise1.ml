(* In this exercise, we want to implement the abstract module
 * type map in different ways.
 *)
module type Map = sig
  type ('a, 'b) t
  val empty : ('a, 'b) t
  val set : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val get : 'a -> ('a, 'b) t -> 'b option
  val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
  val keys : ('a, 'b) t -> 'a list
end

(* Implement a module ListMap, that realizes a map through association lists
 * and fulfills the signature Map.
 *)
(* TODO define ListMap *)

(* Implement a module TreeMap, that realizes a map through a binary search
 * tree and fulfills the signature Map.
 *)
(* TODO define TreeMap *)

(* Implement a functor ExtendMap, that takes a module with signature Map as
 * argument. The functor should provide all definitions of the argument, as
 * well as the following functions:
 val pairs : ('a, 'b) t -> ('a * 'b) list
 val from_pairs : ('a * 'b) list -> ('a, 'b) t
 val values : ('a, 'b) t -> 'b list
 val map_values : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
 val filter_values : ('b -> bool) -> ('a, 'b) t -> ('a, 'b) t
 * Afterwards, define an ExtendedListMap module and an ExtendedTreeMap module.
 *)
(* TODO define ExtendMap, ExtendedListMap, ExtendedTreeMap *)