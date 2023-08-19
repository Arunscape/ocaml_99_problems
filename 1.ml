let rec last l  = match l with
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t;;


last ["a" ; "b" ; "c" ; "d"];;
last [];;
