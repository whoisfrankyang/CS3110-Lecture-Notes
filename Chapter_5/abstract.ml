module type Ring = sig 
  type t 
  val zero : t
  val one : t 
  val ( + ) : t -> t -> t 
  val ( * ) : t -> t -> t 

  val (~- ) : t -> t 

  val string : t -> string  
end 

module IntRingRep = struct 
  type t = int 
  let zero = 0 
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let string = string_of_int
end 

module IntRing : Ring = IntRingRep 

module type Field = sig 
  include Ring 
  val ( / ) : t->t->t
end 
