(* OCaml basics *)

(* FUNCTIONS *)
(* define a function add5 that adds 5 to an integer input *)
let add5 x = x+5

(* RECORDS *)
(* we define the following type for records of students: *)
type student = {age : int; name : string; id : int}

(* define a function student_age that returns the age of a student record *)
let student_age s = s.age

(* define a function inc_age that increases the age of a student by one *)
let inc_age s = {age = s.age+1;name = s.name; id = s.id}

(* PATTERN MATCHING *)
(* pattern matching is used to define the behaviour of a function based on the structure of its arguments. We can match any datatype, but it is most useful with structured types like tuples, lists or trees. *)
let example_int x =
  match x with
    | 0 -> "Zero"
    | n -> "Not Zero"

let example_list l =
  match l with
    | [] -> "List is empty"
    | [x] -> "List contains one element"
    | x::y::ys -> "List contains more than one element"

(* if we want to pattern match on the last paramenter of a function, we can leave it out and instead use the function keyword. *)
let match_version a b c =
  match c with
    | [] -> 0
    | x::xs -> x + a + b
(* is the same as *)
let function_version a b = function
  | [] -> 0
  | x::xs -> x + a + b

(* We can also match multiple patterns with the same function behaviour (as long as the patterns share the same variables) *)
let example_multiple_patterns n = 
  match n with
    | 0 | 1 | 2 | 3 -> "less than 4"
    | 4 | 5 | 6 | 7 -> "less than 8"
    | _ -> "at least 8"

(* We can also use implicit pattern matching in the parameter list of a function. This is especially useful with tuples and other parameters with a fixed structure. *)
let swap_match x =
  match x with
    | (a,b) -> (b,a)
(* is the same as *)
let swap_implicit_match (a,b) = (b,a)

(* define a function head that takes a list and returns its first element *)

(* define a function first3 that takes a list and returns its first three elements as a tuple *)

(* define a function list_from_triple that turns a tuple of three elements into a list *)

(* CUSTOM DATA TYPES *)
(* We can define our own data types using the "type" keyword: *)
type message =
  | Hello
  | Goodbye
  | Text of string

(*
  Define a playing card data type that has constructors without parameters for the card types of Ace, King, Queen and Jack, as well as a constructor for a Number, with an integer parameter which specifies the number of the card. *)
type playing_card = TODO

(* now, we can also match on this new type. Define a function that calculates the value of a card. (Ace is eleven; King, Queen and Jack are ten points each) *)
let value_of_card = failwith "TODO"