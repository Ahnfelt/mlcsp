(* Fibonacci CSP network *)

open Csp
open Legoland

let rec stop n i o () =
    match n with
      | 0 -> Csp.poison i; Csp.poison o; exit 0
      | _ -> Csp.write o (Csp.read i); stop (n-1) i o ()

let _ = 
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
    Csp.parallel [
      fibonacciInt c1;
      stop 42 c1 c2;
      printer c2
    ]
