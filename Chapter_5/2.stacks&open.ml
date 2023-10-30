module type ListStackSig = sig 
  (** ['a stack] is the representation type for stacks*)
  (*the signature does not reveal the type of the 'a stack*)
  type 'a stack
  (** [empty] is the empty stack*)
  val empty : 'a stack

  (** [push x s] is [s] with [x] pushed on top*)
  val push : 'a -> 'a stack -> 'a stack

  (** [is_empty s] is whether [s] is empty*)
  val is_empty : 'a stack -> bool 

  (** [peek s] is the top element of [s]
      Raises Empty exception if [s] is empty*)
  val peek : 'a stack -> 'a
        
  (** [pop s] is [s] with the top element removed
        Raises Empty exception if [s] is empty*)
  val pop : 'a stack -> 'a stack
end 

module ListStack: ListStackSig = struct 
  type 'a stack = 'a list 
  (*[empty] is the empty stack*)
  let empty = []

  (*[is_empty s] check to see whether [s] is empty*)
  let is_empty = function 
    | [] -> true
    | _ -> false 
  
  (*[push x s] pushes [x] onto the top of [s]*)
  let push x s = x :: s 

  (*raised whenan oepration cannoe be applied to an empty stack*)
  exception Empty

  (*[peek s] return the top element of [s]
    Raises Empty exception if [s] is empty*)
  let peek = function 
    | [] -> raise Empty 
    | x :: _ -> x 
  
  (*[pop s] return all but the top element of [s]
     raises Empty exception if [s] is empty*)
  let pop = function 
    | [] -> raise Empty
    | _ :: s -> s

end 

let stack  = ListStack.empty;;
let stack =  ListStack.push 1 stack;;
let stack = ListStack.push 2 stack;;
(* - : int list = [2;1]*)


let x = ListStack. (peek (push 42 empty))
let y = ListStack. (empty |> push 42 |> peek)
(*useful when you have many lines*)

(* local open *)
let w =
  (* let open M in e: makes names from [M] be in scope in [e]*) 
  let open ListStack in 
  empty |> push 42 |> peek

  (* global open: can cause shawdowing if open two modules that contain similar names*)
open ListStack 
let v = empty |> push 42 |> peek