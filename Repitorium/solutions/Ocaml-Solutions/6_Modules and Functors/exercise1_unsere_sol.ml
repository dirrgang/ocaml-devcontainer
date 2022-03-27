(* In this exercise, we want to implement the abstract module
 * type map in different ways.
 *)
module type Map = sig
  type ('a, 'b) t

  val empty : ('a, 'b) t

  (* set a b m -> m (a |-> b) *)
  val set : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

  val get : 'a -> ('a, 'b) t -> 'b option

  val remove : 'a -> ('a, 'b) t -> ('a, 'b) t

  val keys : ('a, 'b) t -> 'a list
end

(* Implement a module ListMap, that realizes a map through association lists
 * and fulfills the signature Map.
 *)
module ListMap : Map = struct
  type ('a, 'b) t = ('a * 'b) list

  let empty = []

  let set a b l = (a, b) :: l

  let rec set k v t =
    match t with
    | [] -> [ (k, v) ]
    (* 1. check if the key already exists*)
    | (k', v') :: xs -> if k' = k then (k, v) :: xs else (k', v') :: set k v xs
  (* overwrıte the key *)

  let get = List.assoc_opt

  let remove = List.remove_assoc

  let keys m = List.map fst m
end

(* Implement a module TreeMap, that realizes a map through a binary search
 * tree and fulfills the signature Map.
 *)
module TreeMap : Map = struct
  type ('a, 'b) t = Leaf | Node of 'a * 'b * ('a, 'b) t * ('a, 'b) t

  let empty = Leaf

  let rec set k v = function
    | Leaf -> Node (k, v, Leaf, Leaf)
    | Node (k', v', l, r) -> (
        match compare k k' with
        | z when z > 0 -> Node (k', v', l, set k v r)
        | 0 -> Node (k, v, l, r)
        | _ -> Node (k', v', set k v l, r))

  let rec get k = function
    | Leaf -> None
    | Node (k', v', l, r) -> (
        match compare k k' with
        | z when z > 0 -> get k r
        | 0 -> Some v'
        | _ -> get k l)

  let rec remove k (Node (k', v', l, r)) =
    let rec remove_rightmost = function
      | Leaf -> (Leaf, None)
      | Node (k, v, l, r) -> (
          match remove_rightmost r with
          | _, None -> (l, Some (k, v))
          | r', Some x -> (Node (k, v, l, r'), Some x))
    in
    match compare k k' with
    | z when z > 0 -> Node (k', v', l, remove k r)
    | 0 -> (
        match remove_rightmost l with
        | _, None -> r
        | l', Some (s, t) -> Node (s, t, l', r))
    | _ -> Node (k', v', remove k l, r)

  let rec keys = function
    | Leaf -> []
    | Node (k, v, l, r) -> keys l @ [ k ] @ keys r
end

(****************nur bis hier :)******************)

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
