(* Some Simple Networks *)

open Csp
open Legoland

let rec numbersint out () =
  let a = Csp.channel () in
  let b = Csp.channel () in
  let c = Csp.channel () in
  let (x,y,z) = Csp.parallel3
    (prefixint 0 (Csp.read c) (Csp.write b))
    (delta2int a out b)
    (succint b c) in
    numbersint out ()    
  
