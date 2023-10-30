let equal lst =
  match lst with 
    | [] -> false
    | _ :: [] -> false
    | h1 :: h2 :: t -> h1 = h2

let equal lst =
  match lst with 
  | h1 :: h2 :: t -> h1 = h2
  | _ -> false