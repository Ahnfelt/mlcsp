(* Fibonacci CSP network *)

open Legoland

let bii x = Big_int.big_int_of_int x
let sbi x = Big_int.string_of_big_int x
let (++) x y = Big_int.add_big_int x y
       
let _ = 
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
      Csp.parallel [
        fibonacciInt c1;
        stop 42 c1 c2;
        printer c2
      ]
