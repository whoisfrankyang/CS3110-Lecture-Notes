module type X = sig 
  val x : int  
end 

module A : X = struct 
  let x = 0 
end 

(*With functor, you must specify the input type of a functor*)
module IncX = functor (M : X) -> struct  
  let x = M.x + 1
end 

module IncX (M : X) = struct 
  let x = M.x + 1
end 



type person  = Child | Teenager | Adult | Senior 
type animal = Dog | Cat | Hamster 
type tenant = Human of person | Pet of animal 
let house : tenant list = [Human(Child);Human(Adult);Pet(Cat)]
