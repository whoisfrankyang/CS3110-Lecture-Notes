module ListQueueImpl = struct
  type 'a queue = 'a list

  let empty = []

  let enqueue x q = q @ [x]

  let peek = function
    | [] -> None
    | x :: _ -> Some x

  let dequeue = function
    | [] -> None
    | _ :: t -> Some t
end

module type Queue = sig 
  (* the type of a queue containing elements of type 'a *)
  type 'a queue 

  (* the empty queue *)
  val empty : 'a queue 

  (* [enqueue x q] is [q] with [x] at the end of the queue*)
  val enqueue : 'a -> 'a queue -> 'a queue

  (* [peek q] is [Some x], where [x] is the front element of [q], or [None] if 
     [q] is empty*)
  val peek : 'a queue -> 'a option 

  (* [dequeue q] is [Some q'], where [q'] is all but the front element of [q],
     or [None] if [q] is empty*)
  val dequeue : 'a queue -> 'a queue option
end

module TwoListQueues: Queue = struct 

  (* front = [a;b]; back = [c;d;e] represents the queue a,b,c,d,e
     if [front] is empty, then [back] must also be empty, to guarantee
      that the first element of the queue is always the head of [front]*)
  type 'a queue = {
    front : 'a list;
    back : 'a list;
  }

  let empty = {
    front = [];
    back = [];
  }

  (*if [front] is empty, then the queue must be empty. We return None
     if [front] has some element, then we just return that element*)
  let peek = function
    | {front = []} -> None 
    | {front = x :: _} -> Some x 
    
  let enqueue x = function 
    (* if [front] of the queue is empty, we enter x to be the first element in [front], and keep
     [back] empty *)
    | {front = []} -> {front = [x]; back = []}
    (*if the queue is not empty, we put x at the head of the [back] of the queue*)
    | q -> {q with back = x :: q.back}

  let dequeue = function 
    | {front = []} -> None 
    (*if there is a head element of the front list, we take that off from the head of the front*)
    (* but if the front is _::[] such that the front will be empty after removal, 
       then we reverse the [back] to make it so be [front] and make [back] = [] *)
    | {front = _::[]; back} -> Some {front = List.rev back; back =[]}

    | {front = _ :: t; back} -> Some {front =t; back}
end 

module ListQueue : Queue = ListQueueImpl
module TwoListQueue : Queue = TwoListQueues
