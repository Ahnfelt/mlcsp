open Cspu
open Legoland

let sbi = Big_int.string_of_big_int

let _ = print_endline ""

let custom_printer i1 i2 () =
  let pl = Cspu.poison_list [pc i1; pc i2] in
  try
    while true do
      Csp.select [
        Csp.read_guard i1 (fun x -> print_endline (sbi x));
        Csp.read_guard i2 (fun x -> print_endline (sbi x));
      ]
    done
  with Csp.PoisonException -> pl raise_poison ()

let custom_stop n i o () =
  let pl = poison_list [pc i] in
  try
    for j = 1 to n do
      Csp.write o (Csp.read i);
    done;
    pl raise_poison ()
  with Csp.PoisonException -> pl raise_poison ()

      
let _ = 
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
  let c3 = Csp.new_channel () in
  let c4 = Csp.new_channel () in
      Csp.parallel [
        numbersInt c1;
        (*stop 5 c1 (Csp.write_only c3);*)
        (*custom_stop 5 c1 (Csp.write_only c3);*)
        custom_stop 20 c1 (Csp.write_only c3);
        numbersInt c2;
        stop 10 c2 c4;
        custom_printer c3 c4
      ]
