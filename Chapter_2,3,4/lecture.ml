let rec power_lin x n =
  if n = 0 then 1
  else x * power_lin x (n-1)

let rec power_log x n =
  if n = 0 then 1
  else if n mod 2 = 0
  then power_log (x*x) (n/2)
  else x * power_log (x*x) (n/2)

let rec sum lst = 
  match lst with
  | [] -> 0
  | h::t -> h + sum t;;

let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h::t -> h::(append t lst2);;


append [1;2;3] [4;5];;
append [] [2;3];;


let rec length lst =
  match lst with
  | [] -> 0
  | h::t -> 1 + length t;;


