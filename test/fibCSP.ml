(* Fibonacci CSP network *)

open Csp
open Legoland

let _ = 
  let c = Csp.channel () in
    Csp.parallel [
      fibonacciInt c;
      printer c
    ]
