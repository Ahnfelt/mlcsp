(* Fibonacci CSP network *)

open Legoland

let _ = print_endline "Fibonacci numbers [1..42]:"
      
let _ = 
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
      Csp.parallel [
        fibonacciInt c1;
        stop 42 c1 c2;
        printer c2
      ]
