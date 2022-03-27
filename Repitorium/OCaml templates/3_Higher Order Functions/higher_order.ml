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

let l1 = [Ace; King; Queen; Jack; Number 8; Number 5]
(* Now we use functions from the List module to implement the sum_values function. We can do this in multiple different ways. *)
let sum_values: playing_card list -> int = fun l -> List.fold_left (fun acc x -> acc + value_of_card x) 0 l

(* 1: use the List.map function to increase all the values in an int list by 0.5 *)
let add_half: playing_card list -> float list = 
  fun l -> List.map (fun x -> float_of_int (value_of_card x) +. 0.5) l

(* 2 : use List.filter to remove all the even numbers from an int list *)
let remove_even = List.filter (fun x -> (value_of_card x) mod 2 <> 0)

(* 3 : use a single fold function to calculate the length of a list *)
let length = List.fold_left (fun acc _ -> acc + 1) 0

(* 4 : Use a single fold function to reverse a list *)
let reverse = List.fold_left (fun acc x -> x :: acc) []

(* 5 : Use functions from the List module to merge two integer lists of the same length and return a list of the sums of the elements at the same position.contents
  Example: merge_sum [1;2;3] [0;1;2] results in [1;3;5] *)

let l2 = [1; 2; 3]
let l3 = [4; 5; 6]

let merge_sum = List.map2 (fun x y -> x + y)

(* 6 : Now use only one fold function from the List module to implement the same functionality *)

(* 7 : Use functions from the List module to test if an int list contains at least one occurance of the integer value 42. *)
let contains_42 = List.exists (fun x -> x == 42)

(* 8 : Use functions from the List module to test if every value in an int list is either 42 or divisible by 17. *)
let all_42_or_div_17 = List.for_all (fun x -> x = 42 || x mod 17 = 0)

(* 9 : Use a single fold function to calculate the (float) average of an int list *)
let average l = (float (List.fold_left (+) 0 l)) /. (float (List.length l))

(* 10 : Use functions from the List module to calculate the median value of a float list *)
let median l = List.nth l ((List.length l) / 2)
let l4 = [1; 1; 2; 4; 37]
(* 11 : Implement your own versions of List.map, once using List.fold_left, and once using List.fold_right *)
let map_fl = failwith "TODO"
let map_fr = failwith "TODO"
