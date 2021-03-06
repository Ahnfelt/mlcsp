(* helper functions for poison propagation *)

let poison_list l fn () = 
  try fn () with e -> List.iter (fun f -> f ()) l; raise e

let poison_raise () =
  raise Csp.PoisonException

let poison_channel c () = Csp.poison c

(* abbreviations *)

let bii = Big_int.big_int_of_int
let sbi = Big_int.string_of_big_int
let (++) = Big_int.add_big_int
let pc = poison_channel

(* processes *)

let terminator i () =
  let pl = poison_list [pc i] in
  try
    while true do
      Csp.read i;
    done
  with Csp.PoisonException -> pl poison_raise ()

let stop n i o () =
  let pl = poison_list [pc i; pc o] in
  try
    for j = 0 to n do
      Csp.write o (Csp.read i);
    done;
    pl poison_raise ()
  with Csp.PoisonException -> pl poison_raise ()
    
let printer i () =
  let pl = poison_list [pc i] in
  try
    while true do
      print_endline(sbi (Csp.read i));
    done
  with Csp.PoisonException -> pl poison_raise ()
        
let idint i o () =
  let pl = poison_list [pc i; pc o] in
  try
    while true do
      Csp.write o (Csp.read i);
    done
  with Csp.PoisonException -> pl poison_raise ()

let succint i o () =
  let pl = poison_list [pc i; pc o] in
  try
    while true do
      Csp.write o (Csp.read i ++ bii 1);
    done
  with Csp.PoisonException -> pl poison_raise ()

let plusint i1 i2 o () =
  let i1' = Csp.new_channel () in
  let i2' = Csp.new_channel () in
  let pl = poison_list [pc i1; pc i2; pc o; pc i1'; pc i2'] in
  try 
    while true do
      Csp.parallel [
        pl (fun () -> Csp.write i1' (Csp.read i1));
        pl (fun () -> Csp.write i2' (Csp.read i2));
        pl (fun () -> Csp.write o (Csp.read i1' ++ Csp.read i2'))
      ];
    done 
  with Csp.PoisonException -> pl poison_raise ()
      
let rec delta2int i o1 o2 () =
  let pl = poison_list [pc i; pc o1; pc o2;] in
  try
    while true do
      let x = Csp.read i in
        Csp.parallel[
          pl (fun () -> Csp.write o1 x);
          pl (fun () -> Csp.write o2 x);
        ];
    done
  with Csp.PoisonException -> pl poison_raise ()

let prefixint n i o () =
  let pl = poison_list [pc i; pc o] in
  try
    Csp.write o (n);
    idint i o ()
  with Csp.PoisonException -> pl poison_raise ()

let tailint i o () =
  let pl = poison_list [pc i; pc o] in
  try
    Csp.read i; (* we drop first number *)
    while true do
      idint i o ()
    done
  with Csp.PoisonException -> pl poison_raise ()

let blockingFifo i o () =
  let c = Csp.new_channel () in
    Csp.parallel [
      idint i c;
      idint c o
    ]


(* Some Simple Networks *)

let numbersInt o () =
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
  let c3 = Csp.new_channel () in
    Csp.parallel [
      prefixint (bii 0) c3 c1;
      delta2int c1 o c2;
      succint c2 c3
    ]
      
let integrateInt i o () =
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
  let c3 = Csp.new_channel () in
    Csp.parallel [
      plusint i c3 c1;
      delta2int c1 o c2;
      prefixint (bii 0) c2 c3
    ]
      
let pairsInt i o () =
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
  let c3 = Csp.new_channel () in
    Csp.parallel [
      delta2int i c1 c2;
      tailint c1 c3;
      plusint c3 c2 o
    ]

      
(* Some Layered Networks *)

let fibonacciInt o () =
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
  let c3 = Csp.new_channel () in
  let c4 = Csp.new_channel () in
    Csp.parallel [
      prefixint (bii 1) c4 c1 ;
      prefixint (bii 0) c1 c2 ;
      delta2int c2 o c3;
      pairsInt c3 c4
    ]

let squaresInt o () =
  let c1 = Csp.new_channel () in
  let c2 = Csp.new_channel () in
    Csp.parallel [
      numbersInt c1;
      integrateInt c1 c2;      
      pairsInt c2 o
    ]

