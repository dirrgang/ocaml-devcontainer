(* For each exercise, write a recursive function that implements the expected functionality.
You may NOT use any library functions from the List module (map,fold,...) *)

(* 1 : Recall the playing cards data type and the value_of_card function we defined earlier. *)
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

(* now, we define a function that computes the sum of the values of the cards in a list. *)
let rec sum_values = function
  | [] -> 0
  | x::xs -> value_of_card x + sum_values xs


(* 2 : add_half adds 0.5 to every integer value in the list.
  Note: you need to convert the integers to floats.
  Example: add_half [1;-4;0] results in [1.5;-3.5;0.5] *)
let rec add_half = function
  | [] -> []
  | x::xs -> ((float_of_int x) +. 0.5) :: add_half xs

(* 3 : reverse reverses the list.
  Example: reverse [1;-4;0] results in [0;-4;1] *)
let rec reverse = function
  | [] -> []
  | x::xs -> reverse xs @ [x]

  (* 4 : define a function nth, such that nth i l returns the ith element of the list l as an optional value. nth returns None if there is no such element. You can ignore negative inputs for i.
  Examples:
    nth 1 [1;-4;0] results in Some (-4)
    nth 3 [1;-4;0] results in None *)
  let rec nth i = function
    | [] -> None
    | x::xs -> if i = 0 then Some x else nth (i - 1) xs

let rec nth i l = match i,l with
  | j, [] -> None
  | 0, x::xs -> Some x
  | j, x::xs -> nth (j-1) xs

let and_function a b =
  match (a,b) with
    | (true,true) -> true
    | _ -> false

(* 5 : define an "album" record type that stores the following information:
  the album title (title),
  the year of release as an integer value (release_year),
  the song titles and their individual lengths as a list of tuples (songs)*)
type album = {title: string; release_year: int ; songs: (string*float) list}

(* now, define a function album_length that calculates the total length of an album *)
let album_length album = 
  let rec length_of_songs = function
    | [] -> 0.
    | (song,length)::xs -> length +. length_of_songs xs
  in length_of_songs album.songs

let f x = let a = x*x in a + a

(* 6 : define a function oldest_album that returns the title of the album with the earliest release_year from a list of albums. If there are multiple such albums, return the title of the first album with the earliest release_year. You can assume the list to be non-empty.
  Hint: you might want to use a helper function here *)
  let oldest_album alist = 
    let rec inner elem xs = match xs with 
      | [] -> elem.title
      | x::xs -> if elem.release_year <= x.release_year then inner elem xs else inner x xs
    in
    match alist with
      | [] -> failwith "empty list"
      | x::xs -> inner x xs

(* 7 : define a function average which calculates the average (float) value of all integer values in the list.
  Hint: you might want to use a helper function here.
  Example: average [1;-4;0] results in -1.0 *)
  let average l = 
    let rec inner sum count xs = match xs with
      | [] -> Int.to_float sum /. Int.to_float count
      | x::xs -> inner (sum+x) (count+1) xs
    in
    inner 0 0 l
