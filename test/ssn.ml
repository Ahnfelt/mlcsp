(* Some Simple Networks *)

open Legoland

let _ = print_endline "Numbers from [0..41]:"

let _ =
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
  Csp.parallel [
    numbersInt c1;
    stop 42 c1 c2;
    printer c2
  ]

let _ = print_endline "Squares from [1..41]:"

let _ =
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
  Csp.parallel [
    squaresInt c1;
    stop 42 c1 c2;
    printer c2
  ]
