(* $Id: legoland.ml,v 1.0 2009/05/05 09:00:00 gentauro Exp $ *)

let bii x = Big_int.big_int_of_int x
let sbi x = Big_int.string_of_big_int x
let (++) x y = Big_int.add_big_int x y

let rec terminator i () =
  try
    Csp.read i;
    terminator i ()
  with Csp.PoisonException -> Csp.poison i

let rec printer i () =
  try
    let x = Csp.read i in
      print_endline(sbi x);
      printer i ()
  with Csp.PoisonException -> Csp.poison i
        
let rec idint i o () =
  try
    Csp.write o (Csp.read i);
    idint i o ()
  with Csp.PoisonException -> Csp.poison i; Csp.poison o

let rec succint i o () =
  try
    Csp.write o (Csp.read i ++ bii 1);
    succint i o ()
  with Csp.PoisonException -> Csp.poison i; Csp.poison o

let rec plusint i1 i2 o () =
  let i1' = Csp.channel () in
  let i2' = Csp.channel () in
    try
      Csp.parallel [
        (fun () -> Csp.write i1' (Csp.read i1));
        (fun () -> Csp.write i2' (Csp.read i2));
        (fun () -> Csp.write o (Csp.read i1' ++ Csp.read i2'));
      ];
      plusint i1 i2 o ()
  with Csp.PoisonException ->
    Csp.poison i1; Csp.poison i2; Csp.poison o;
    Csp.poison i1'; Csp.poison i2'

let rec delta2int i o1 o2 () =
  try
    let x = Csp.read i in
      Csp.parallel[
        (fun () -> Csp.write o1 (x));
        (fun () -> Csp.write o2 (x))
      ];delta2int i o1 o2 ()
  with Csp.PoisonException -> Csp.poison i; Csp.poison o1; Csp.poison o2

let rec prefixint n i o () =
  try
    Csp.write o (n);
    idint i o ()
  with Csp.PoisonException -> Csp.poison i; Csp.poison o

let rec tailint i o () =
  try
    Csp.read i; (* we drop first number *)
    idint i o ()
  with Csp.PoisonException -> Csp.poison i; Csp.poison o

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
