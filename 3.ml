(* 1 indexed eww *)
let rec at k = function
  | [] -> None
  | car :: cdr ->
      match k with
      | 1 -> Some car
      | _ -> at (k - 1) cdr;;

at 3 ["a"; "b"; "c"; "d"; "e"];;
at 3 ["a"];;
