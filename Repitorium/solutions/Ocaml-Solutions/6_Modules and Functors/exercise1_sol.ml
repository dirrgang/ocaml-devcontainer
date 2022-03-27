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
module ListMap : Map = struct
  type ('a, 'b) t = ('a * 'b) list

  let empty = []

  let set a b l = (a, b) :: l

  let get = List.assoc_opt

  let remove = List.remove_assoc

  let keys l = fst (List.split l)
end

(* Implement a module TreeMap, that realizes a map through a binary search
 * tree and fulfills the signature Map.
 *)
module TreeMap : Map = struct
  type ('a, 'b) t = Leaf | Node of 'a * 'b * ('a, 'b) t * ('a, 'b) t

  let empty = Leaf

  let rec set a b = function
    | Leaf -> Node (a, b, Leaf, Leaf)
    | Node (k, v, l, r) -> (
        match compare a k with
        | z when z > 0 -> Node (k, v, l, set a b r)
        | 0 -> Node (a, b, l, r)
        | _ -> Node (k, v, set a b l, r))

  let rec get a = function
    | Leaf -> None
    | Node (k, v, l, r) -> (
        match compare a k with
        | z when z > 0 -> get a r
        | 0 -> Some v
        | _ -> get a l)

  let rec remove a (Node (k, v, l, r)) =
    let rec remove_rightmost = function
      | Leaf -> (Leaf, None)
      | Node (k, v, l, r) -> (
          match remove_rightmost r with
          | _, None -> (l, Some (k, v))
          | r', Some x -> (Node (k, v, l, r'), Some x))
    in
    match compare a k with
    | z when z > 0 -> Node (k, v, l, remove a r)
    | 0 -> (
        match remove_rightmost l with
        | _, None -> r
        | l', Some (k', v') -> Node (k', v', l', r))
    | _ -> Node (k, v, remove a l, r)

  let rec keys = function
    | Leaf -> []
    | Node (k, _, l, r) -> keys l @ [ k ] @ keys r
end

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
module ExtendMap (X : Map) = struct
  include X

  let pairs xs = X.keys xs |> List.map (fun x -> (x, Option.get (X.get x xs)))

  let from_pairs xs = List.fold_left (fun a (k, v) -> X.set k v a) X.empty xs

  let values xs = X.keys xs |> List.map (fun x -> Option.get (X.get x xs))

  (* alternatively *)
  let values xs = snd (List.split (pairs xs))

  let map_values f xs =
    List.combine (X.keys xs) (values xs |> List.map f) |> from_pairs

  let filter_values f xs =
    pairs xs |> List.filter (fun (k, v) -> f v) |> from_pairs
end

module ExtendedListMap = ExtendMap (ListMap)
module ExtendedTreeMap = ExtendMap (TreeMap)
