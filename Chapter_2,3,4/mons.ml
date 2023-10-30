type ptype = TNormal | TFire | TWater

type peff = ENormal | ENotVery | ESuper 

(* OCaml also has shadowing*)

(* produce a multiplier out of the effectiveness*)
let mult_of_eff = function 
  | ENormal -> 1.
  | ENotVery -> 0.5
  | ESuper -> 2.

let eff = function 
  | (TFire, TFire) | (TWater, TWater) | (TFire, TWater) -> ENotVery 
  | (TWater, TFire) -> ESuper
  | _ -> ENormal


type mon = {
  name : string;
  hp : int;
  ptype : ptype;
  }

let charmander = {
  name = "Charmander";
  hp = 39;
  ptype = TFire;
}