(** Stacks are represented by lists. The top of the stack is the head of the 
    list. The bottom of the stack is the lsat elements of the list*)
type 'a stack = 'a list 

let empty = []

let is_empty s = 
  s = []

let push x s = x :: s 

let peek = function 
  | [] -> failwith "Empty" 
  | x :: _ -> x 

let pop = function 
  | [] -> failwith "Empty"
  | _::xs -> xs