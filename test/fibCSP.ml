(* Fibonacci CSP network *)

open Csp
open Legoland

let rec numbers out n () =
  Csp.write out (n); numbers out (n + 1) ()

let rec print inp () =
  let x = Csp.read inp in
    print_endline(string_of_int x); print inp ()

let rec fibcsp n =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
  let out = Csp.channel () in
  Csp.fork [
    prefixint 0 c3 c1;
    delta2int c1 out c2;
    succint c2 c3;
    print out
  ]


let numbersInt out () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
  let c4 = Csp.channel () in
  Csp.fork [
    prefixint 0 c3 c1;
    delta2int c1 c4 c2;
    succint c2 c3;
    fun () -> (while true do (Csp.write out (Csp.read c4)); done)
  ]

let _ =
  let c = Csp.channel () in
  Csp.fork [
    (numbersInt c);
    (print c)
  ]


(* 
working nice :D

let _ = fibcsp (10)
*)

(*
pretty ugly with parallel, we get a huge tuple

let _ =
  let c = Csp.channel () in
  let _ = Csp.parallel
    (numbers c 1)
    (print c) in
    ()
*)

(*
pretty nice with fork

let _ =
  let c = Csp.channel () in
  Csp.fork [
    (numbers c 1);
    (print c)
  ]
*)
