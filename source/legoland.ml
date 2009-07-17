(* $Id: legoland.ml,v 1.0 2009/05/05 09:00:00 gentauro Exp $ *)

open Csp.Utilities

(* abbreviations *)
let bii = Big_int.big_int_of_int
let sbi = Big_int.string_of_big_int
let (++) = Big_int.add_big_int
let pc = poison_channel

let rec terminator i () =
  let pl = poison_list [pc i] in
  try 
    Csp.read i;
    terminator i ()
  with Csp.PoisonException -> pl raise_poison ()

let rec stop n i o () =
  let pl = poison_list [pc i; pc o] in
  match n with
    | 0 -> pl raise_poison();
    | _ ->
        try 
          Csp.write o (Csp.read i);
          stop (n-1) i o ()
        with Csp.PoisonException -> pl raise_poison ()
    
let rec printer i () =
  let pl = poison_list [pc i] in
  try
    let x = Csp.read i in
      print_endline(sbi x);
      printer i ()
  with Csp.PoisonException -> pl raise_poison ()
        
let rec idint i o () =
  let pl = poison_list [pc i; pc o] in
  try
    Csp.write o (Csp.read i);
    idint i o ()
  with Csp.PoisonException -> pl raise_poison ()

let rec succint i o () =
  let pl = poison_list [pc i; pc o] in
  try
    Csp.write o (Csp.read i ++ bii 1);
    succint i o ()
  with Csp.PoisonException -> pl raise_poison ()

let rec plusint i1 i2 o () =
  let i1' = Csp.channel () in
  let i2' = Csp.channel () in
  let pl = poison_list [pc i1; pc i2; pc o; pc i1'; pc i2'] in
    Csp.parallel [
      pl (fun () -> Csp.write i1' (Csp.read i1));
      pl (fun () -> Csp.write i2' (Csp.read i2));
      pl (fun () -> Csp.write o (Csp.read i1' ++ Csp.read i2'))
    ];
    if(Csp.poisoned o ||Csp.poisoned i1||Csp.poisoned i2)
    then pl raise_poison () else plusint i1 i2 o ()
      
let rec delta2int i o1 o2 () =
  let pl = poison_list [pc i; pc o1; pc o2;] in
  try
    let x = Csp.read i in
      Csp.parallel[
        pl (fun () -> Csp.write o1 x);
        pl (fun () -> Csp.write o2 x);
      ];delta2int i o1 o2 ()
  with Csp.PoisonException -> pl raise_poison ()

let rec prefixint n i o () =
  let pl = poison_list [pc i; pc o] in
  try
    Csp.write o (n);
    idint i o ()
  with Csp.PoisonException -> pl raise_poison ()

let rec tailint i o () =
  let pl = poison_list [pc i; pc o] in
  try
    Csp.read i; (* we drop first number *)
    idint i o ()
  with Csp.PoisonException -> pl raise_poison ()

let blockingFifo i o () =
  let c = Csp.channel () in
    Csp.parallel [
      idint i c;
      idint c o;
    ]


(* Some Simple Networks *)

let numbersInt o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
    Csp.parallel [
      prefixint (bii 0) c3 c1;
      delta2int c1 o c2;
      succint c2 c3
    ]
      
let integrateInt i o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
    Csp.parallel [
      plusint i c3 c1;
      delta2int c1 o c2;
      prefixint (bii 0) c2 c3
    ]
      
let pairsInt i o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
    Csp.parallel [
      delta2int i c1 c2;
      tailint c1 c3;
      plusint c3 c2 o;
    ]

      
(* Some Layered Networks *)

let fibonacciInt o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
  let c4 = Csp.channel () in
    Csp.parallel [
      prefixint (bii 1) c4 c1 ;
      prefixint (bii 0) c1 c2 ;
      delta2int c2 o c3;
      pairsInt c3 c4;
    ]

let squaresInt o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
    Csp.parallel [
      numbersInt c1;
      integrateInt c1 c2;      
      pairsInt c2 o;
    ]

