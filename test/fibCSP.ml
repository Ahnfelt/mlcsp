(* Fibonacci CSP network *)

open Csp
open Legoland

(*
some local crap

let rec numbers out n () =
  Csp.write out (n); numbers out (n + 1) ()

let rec print inp () =
  let x = Csp.read inp in
    print_endline(string_of_int x); print inp ()

let rec fibcsp n = n + 1
*)

(*
square of ints also working great after refactoring
let _ =
  let c = Csp.channel () in
  Csp.parallel [
    squaresInt c;
    printer c
  ]

*)

(*
Juhu ... fibonacci is working great (we need to use big ints)
*)

(*
pairsInt with numbersInt

let _ =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  Csp.parallel [
    numbersInt c1;
    pairsInt c1 c2;
    printer c2
  ]
*)

(*
integrateInt with numbersInt

let _ =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  Csp.parallel [
    numbersInt c1;
    integrateInt c1 c2;
    printer c2
  ]
*)

(*
numbersInt with blockinFifo

let _ =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  Csp.parallel [
    numbersInt c1;
    blockingFifo c1 c2;
    printer c2
  ]
*)


(* 
working nice :D

let _ = fibcsp (10)
*)

(*
pretty ugly with parallel, we get a huge tuple

let _ =
  let c = Csp.channel () in
  let _ = Csp.parallel
    numbers c 1
    printer c in
    ()
*)

(*
pretty nice with fork

let _ =
  let c = Csp.channel () in
  Csp.parallel [
    numbers c 1;
    printer c
  ]
*)
