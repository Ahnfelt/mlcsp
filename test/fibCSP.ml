open Legoland

let _ = print_endline "Fibonacci numbers [1..42]:"
      
let _ = 
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
      try Csp.parallel [
        fibonacciInt c1;
        stop 42 c1 c2;
        printer c2
      ] with Csp.PoisonException -> ()

