let x = 
  match not true with
  | true -> "nope"
  | false -> "yep";;

let y =
  match 42 with
  | fooo -> fooo;;

let z =
  match "foo" with
  | "bar" -> 0
  | _ -> 1;;


let a =
  match [] with
  | [] -> "empty"
  | _ -> "not empty";;

let b =
  match ["taylor"; "swift";] with
  | [] -> "folklore"
  | h::t -> h;;

let fst3 t =
  match t with
  | (a,b,c) -> a;;

let empty lst = 
    match lst with
    | [] -> true
    | _-> false;;

let empty_function = function
    | [] -> true
    | _-> false;;

  
let rec sum lst = 
    match lst with
    | [] -> 0
    | h::t -> h + sum t;;

let rec sum_function = function
    | [] -> 0
    | h::t -> h + sum t;;

let rec length lst =
    match lst with
    | [] -> 0
    | h::t -> 1+ length t;;
  
let rec length_function = function
    | [] -> 0
    | h::t -> 1+ length t;;

(*append [1;2;3] [4;5;6] is [1;2;3;4;5;6]*)
let rec append lst1 lst2 = 
    match lst1 with
    | [] -> lst2
    | h::t -> h::append t lst2;;
  