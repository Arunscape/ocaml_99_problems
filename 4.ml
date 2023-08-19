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
