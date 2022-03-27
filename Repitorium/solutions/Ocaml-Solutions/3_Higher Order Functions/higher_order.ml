(* Implement the following functionalities using only higher order functions (e.g., from the List module). Note: You may NOT define any recursive functions yourself! *)
(* We again use the playing cards example. *)
type playing_card =
  | Ace
  | King
  | Queen
  | Jack
  | Number of int

let value_of_card = function
  | Ace -> 11
  | King | Queen | Jack -> 10
  | Number x -> x

(* Now we use functions from the List module to implement the sum_values function. We can do this in multiple different ways. *)
let sum_values l =
  List.fold_left (fun acc x -> acc + value_of_card x) 0 l
let sum_values2 l = List.map value_of_card l |> List.fold_left (+) 0

(* 1: use the List.map function to increase all the values in an int list by 0.5 *)
let add_half = List.map (fun x -> float_of_int x +. 0.5)

(* 2 : use List.filter to remove all the even numbers from an int list *)
let remove_even l = List.filter (fun x -> x mod 2 <> 0) l

(* 3 : use a single fold function to calculate the length of a list *)
let length l = List.fold_left (fun acc curr -> acc + 1) 0 l

(* 4 : Use a single fold function to reverse a list *)
let reverse l = List.fold_left (fun acc curr -> curr::acc) [] l

(* 5 : Use functions from the List module to merge two integer lists of the same length and return a list of the sums of the elements at the same position.
  Example: merge_sum [1;2;3] [0;1;2] results in [1;3;5] *)
let merge_sum xs ys = List.map2 ( fun x1 x2 -> x1 + x2 ) xs ys

(* 6 : Now use only one fold function from the List module to implement the same functionality *)
let merge_sum_alt l1 l2 =
  List.fold_right2 (fun x y acc-> (x+y)::acc) l1 l2 []

let merge_sum_3 l1 l2=
  List.fold_left2 (fun acc x y -> (x+y)::acc) [] l1 l2 |> List.rev

(* 7 : Use functions from the List module to test if an int list contains at least one occurance of the integer value 42. *)
let contains_42 l = List.exists ( fun x -> x = 42 ) l

(* 8 : Use functions from the List module to test if every value in an int list is either 42 or divisible by 17. *)
let all_42_or_div_17 = List.for_all (fun x -> x = 42 || x mod 17 == 0)

(* 9 : Use a single fold function to calculate the (float) average of an int list *)
let average l1 = 
  (List.fold_left (fun acc x -> acc +. (float_of_int x)) 0. l1)
  /. (float_of_int (length l1))

let avg l =
  match List.fold_left (fun (sum,length) curr -> (sum+curr,length+1)) (0,0) l with
  (s,l) -> (float s) /. (float l)
(* 10 : Use functions from the List module to calculate the median value of a float list *)
let median (l:'a list) = List.nth (List.sort compare l) ((List.length l)/2)

(* 11 : Implement your own versions of List.map, once using List.fold_left, and once using List.fold_right *)
let map_fl f l =
  List.rev (List.fold_left (fun acc a -> (f a)::acc) [] l)
let map_fr f l = List.fold_right (fun a acc -> (f a)::acc) l []
