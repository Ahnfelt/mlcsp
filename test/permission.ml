open Cspu
open Legoland

let sbi = Big_int.string_of_big_int

let _ = print_endline ""

let custom_printer i1 i2 () =
  try
    while true do
      Csp.select [
        Csp.read_guard i1 (fun x -> print_endline (sbi x));
        Csp.read_guard i2 (fun x -> print_endline (sbi x));
      ]
    done
  with Csp.PoisonException -> Csp.poison i1; Csp.poison i2

let custom_stop n i o () =
  try
    for j = 1 to n do
      Csp.write o (Csp.read i);
    done;
    Csp.poison i
  with Csp.PoisonException -> Csp.poison i

(* permission - compiler error when uncommented *)
let _ = 
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
  let c3 = Csp.new_channel () in
  let c4 = Csp.new_channel () in
      Csp.parallel [
        numbersInt c1;
        (* stop 5 c1 (Csp.write_only c3); *)
        numbersInt c2;
        stop 10 c2 c4;
        custom_printer c3 c4
      ]

(* alternation stop first process with 5 *)
let _ = 
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
  let c3 = Csp.new_channel () in
  let c4 = Csp.new_channel () in
      Csp.parallel [
        numbersInt c1;
        custom_stop 5 c1 (Csp.write_only c3);
        numbersInt c2;
        stop 10 c2 c4;
        custom_printer c3 c4
      ]

(* alternation stop first process with 20 *)
let _ = 
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
  let c3 = Csp.new_channel () in
  let c4 = Csp.new_channel () in
      Csp.parallel [
        numbersInt c1;
        custom_stop 20 c1 (Csp.write_only c3);
        numbersInt c2;
        stop 10 c2 c4;
        custom_printer c3 c4
      ]
