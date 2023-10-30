let rec filter p = function 
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let rec filter_aux p  acc = function 
  | [] ->List.rev acc 
  | h :: t -> 
      filter_aux p (if p h then h :: acc else acc) t 