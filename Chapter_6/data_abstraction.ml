module type Set = sig 
  (** ['a t] is the type of a set whose elements have tyep ['a]. *)
  type 'a t 

  (** [empty] is the empty set. *)
  val empty : 'a t 

  (** [size s] is the number of elements in [s]. 
      [size empty] is [0].*)
  val size : 'a t -> int

  (** [add x s] is a set containing all the elements of [s] as well as element [x]. *)
  val add : 'a -> 'a t -> 'a t 

  (** [mem x s] is [true] iff [x] is an element of [s].*)
  val mem : 'a -> 'a t -> bool 

  (** [union s1 s2] is the set containing both the elements
      of [s1] and the elements of [s2]*)
  val union : 'a t -> 'a t -> 'a t

end 




module ListSetNoDups : Set = struct 
  (** AF: The list [a1; ...; an] represents the set {a1, ..., an}.
      The empty lsit [[]] represents the empty set
      RI: The list must not contain duplicates*)
  type 'a t = 'a list

  let rep_ok lst = 
    if List.length lst = List.length (dedup lst) then lst else 
      failwith "RI"

  let empty = []

  let size = List.length
  
  let mem = List.mem
  
  let add x s = 
    let s = rep_ok s in
    if mem x s then s else rep_ok(x :: s)
  
  let union s1 s2 =
    (rep_ok s1) @ (rep_ok s2) |> List.sort_uniq Stdlib.compare

  
end 

(** [dedup lst] is [lst] but with duplicates removed. It also sorts the output lst.*)
let dedup lst = 
  lst |> List.sort_uniq Stdlib.compare 

module ListSetDups : Set = struct 
  (** AF: The list [a1; ...; an] represents the set {b1, ..., bm}
      where [b1; ...; bm] is the same list as [a1; ...; an]
      but with any duplicates removed.
      The empty list [[]] represents the empty set
      RI: None: The list may contain duplicates*)
  type 'a t = 'a list

  let empty = []
  
  let mem = List.mem
  
  let rec size s =
    s |> dedup |> List.length

  let add = List.cons 

  let union = List.append 

end