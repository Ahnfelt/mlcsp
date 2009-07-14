(* Fibonacci CSP network *)

open Legoland

let bii x = Big_int.big_int_of_int x
let sbi x = Big_int.string_of_big_int x
let (++) x y = Big_int.add_big_int x y

let rec stop n i o () =
  match n with
    | 0 -> Csp.poison i; Csp.poison o; (* exit 0 *)
    | _ -> Csp.write o (Csp.read i); stop (n-1) i o ()
        
let _ = 
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
      Csp.parallel [
        (* numbersInt c1; *)
        fibonacciInt c1;
        (* (fun () -> for x = 1 to 44 do Csp.write c1 (bii x) done); *)
        stop 42 c1 c2;
        printer c2
      ]
