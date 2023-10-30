let rec add1 = function 
  | [] -> [] 
  | h :: t -> (h + 1) :: add1 t;;

let rec concat3110 = function 
  | [] -> []
  | h :: t -> (h ^ "3110") :: concat3110 t;;

add1 [1;2;3;4;5];;
concat3110 ["a";"b";"c"];;

let rec map f = function 
  | [] -> [] 
  | h :: t -> f h :: map f t 

let add1' lst= map (fun x -> x+1) lst 
let concat3110' lst = map (fun x -> x ^ "3110") lst

let stringlist_of_intlist lst = map string_of_int lst 

