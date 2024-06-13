(* run:
   utop -init main.ml 
   then you can run these functions and see the result
   *)

(* Tail of a List *)
let rec last l  = match l with
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t;;


last ["a" ; "b" ; "c" ; "d"];;
last [];;


(* Last Two Elements of a List *)
let rec last_two = function
  | [] -> None
  | [x] -> None
  | [x;y] -> Some (x, y)
  | _ :: t -> last_two t;;


last_two ["a" ; "b" ; "c" ; "d"];;
last_two ["a"];;

(* N'th Element of a List *)
(* 1 indexed eww *)
let rec at k = function
  | [] -> None
  | car :: cdr ->
      match k with
      | 1 -> Some car
      | _ -> at (k - 1) cdr;;

at 3 ["a"; "b"; "c"; "d"; "e"];;
at 3 ["a"];;

(* Length of a List*)
let length l =
  let rec length_helper l n = match l with
    | [] -> n
    | _ :: tail -> length_helper tail (n + 1)
  in length_helper l 0;;

length ["a"; "b"; "c"];;
length [];;

(* their example
* # (* This function is tail-recursive: it uses a constant amount of
*      stack memory regardless of list size. *)
*   let length list =
*     let rec aux n = function
*       | [] -> n
*       | _ :: t -> aux (n + 1) t
*     in aux 0 list;;
* val length : 'a list -> int = <fun>
*)


(* Reverse a List *)
let rec rev l =
  let rec rev_helper acc = function
    | [] -> acc
    | h :: t -> rev_helper (h::acc) t
  in
  rev_helper [] l;;


(* Palindrome *)
let is_palindrome l =
  l = rev l;;



(* Flatten *)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let rec flatten l =
  let rec aux acc = function
    | [] -> acc
    | One h :: t -> aux (h :: acc) t
    | Many h :: t -> aux (aux acc h) t
  in
  aux [] l |> rev;;


(* Eliminate Duplicates
  Eliminate consecutive duplicates of list elements *)
let rec compress = function
    | a :: (b :: t) -> if a = b then compress (b::t) else a :: compress (b::t)
    | x -> x


(* this way is more efficient since "as t" makes a binding
https://stackoverflow.com/questions/26769403/as-keyword-in-ocaml
*)
let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | x -> x


(* Pack Consecutive Duplicates *)
let pack l =
  let rec aux curr acc = function
    | [] -> []
    | [x] -> (x::curr) :: acc
    | a :: (b :: _ as t) -> 
      if a = b then aux (a::curr) acc t
      else aux [] ((a::curr) :: acc) t (* new current list *)
  in
  aux [] [] l |> rev

