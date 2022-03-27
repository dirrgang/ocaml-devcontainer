(* In OCaml, there is a pendant to interface implementations: Modules.
 * Modules are a collection of types and values (e.g. functions).
 * Their types are called "module types", and module types can be thought
 * of as interfaces. So: modules are implementations of module types.
 *
 * We already used some modules: e.g. the List module, but also every .ml
 * file provides a module: e.g. "myFile.ml" provides the module "MyFile".
 *
 * Examples...
 *)

(* Defining a module type *)
module type ModuleType = sig
  type 'a type1

  type 'a type2 = 'a list
  (* realization of type2 is exposed in the signature.
     also forces all modules with this module type to implement type2 as list.
     This is also referred to as "concrete type". *)

  val fun1 : 'a type1 -> 'a type1 -> 'a type1

  val fun2 : 'a type1 -> 'a type2

  val some_constant : int
end

(* Implementing a module of a module type *)
module Module : ModuleType = struct
  type 'a type1 = 'a

  type 'a type2 = 'a list

  let fun1 x y = x

  let fun2 x = [ x ]

  let some_constant = 3
end

(* Implementing any module without specifying its type *)
module Module2 = struct
  type 'a m2 = 'a list

  let f : 'a m2 -> 'a m2 = fun x -> x @ x
end

(* opening a module (allows access without qualifying the module, e.g.
   map instead of List.map) *)
open List

(* including a module (opens a module and makes it part of the current module,
 * i.e. similar to "extends" in Java) *)
module MyListModule = struct
  include List

  let rec snoc x = function [] -> [ x ] | y :: ys -> y :: snoc x ys
end

let _ = MyListModule.cons 4 [ 1; 2; 3 ]

let _ = MyListModule.snoc 4 [ 1; 2; 3 ]

(* Functors are like generic modules - or functions mapping modules to modules. *)
(* functor type definition *)
module type FunctorType = functor (X : ModuleType) -> sig
  type t

  val f : 'a X.type1 -> 'a X.type2
end

(* implementing a functor of a certain functor type *)
module Functor : FunctorType =
functor
  (X : ModuleType)
  ->
  struct
    type t = int list

    let f x = X.fun2 x
  end

(* alternatively *)
module type ResultingModuleType = sig
  type t

  val f : int -> int
end

module Functor2 (X : ModuleType) : ResultingModuleType = struct
  type t = bool list

  let f x = X.some_constant + x
end

(* sharing constraints: expose the implementation of a certain type *)
module M : ModuleType with type 'a type1 = int = struct
  type 'a type1 = int

  type 'a type2 = 'a list

  let fun1 x y = x + y

  let fun2 (x : 'a type1) = []

  let some_constant = 1
end

(* If a functor is defined, we can applied it to a module to obtain a module.
 * This is similar to inserting concrete values into generics. *)
module M1 = Functor (Module)
module M2 = Functor2 (Module)
