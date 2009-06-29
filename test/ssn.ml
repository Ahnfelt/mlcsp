(* Some Simple Networks *)

open Csp
open Legoland

let _ =
  let c = Csp.channel () in
  Csp.parallel [
    numbersInt c;
    printer c
  ]
