(*******************************************************************************)
(*                                                                             *)
(*                       Objective Caml CSP Library                            *)
(*                                                                             *)
(*Copyleft (l) 2009 Joakim Ahnfelt-Rønne and Ramón Salvador Soto Mathiesen     *)
(*                                                                             *)
(*Permission is hereby granted, free of charge, to any person obtaining a copy *)
(*of this software and associated documentation files (the "Software"), to deal*)
(*in the Software without restriction, including without limitation the rights *)
(*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    *)
(*copies of the Software, and to permit persons to whom the Software is        *)
(*furnished to do so, subject to the following conditions:                     *)
(*                                                                             *)
(*The above copyright notice and this permission notice shall be included in   *)
(*all copies or substantial portions of the Software.                          *)
(*                                                                             *)
(*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   *)
(*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     *)
(*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  *)
(*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       *)
(*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,*)
(*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN    *)
(*THE SOFTWARE.                                                                *)
(*                                                                             *)
(*******************************************************************************)

(* $Id: legoland.ml,v 1.0 2009/05/05 09:00:00 gentauro Exp $ *)

open Csp

let rec terminator i () =
  terminator i ()

let rec printer i () =
  let x = Csp.read i in
    print_endline(string_of_int x);
    printer i ()
  
let rec idint i o () =
  Csp.write o (Csp.read i);
  idint i o ()

let rec succint i o () =
  Csp.write o ((Csp.read i) + 1);
  succint i o ()

let rec plusint i1 i2 o () =
  let (x, y) = Csp.parallel
    (fun () -> Csp.read i1)
    (fun () -> Csp.read i2) in
    Csp.write o (x + y);
    plusint i1 i2 o ()
      
let rec delta2int i o1 o2 () =
  let x = Csp.read i in
  Csp.fork[
    (fun () -> Csp.write o1 (x));
    (fun () -> Csp.write o2 (x))
  ];delta2int i o1 o2 ()

let rec prefixint n i o () =
  Csp.write o (n);
  idint i o ()

let rec tailint i o () =
  Csp.read i; (* we drop first number *)
  idint i o ()

let blockingFifo i o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
    Csp.fork [
      (* we need some kind of a fn wrapper to make this pretty :/ *)
      (fun () -> (while true do (Csp.write c1 (Csp.read i)); done));
      idint c1 c2;
      idint c2 c3;
      (* we need some kind of a fn wrapper to make this pretty :/ *)
      (fun () -> (while true do (Csp.write o (Csp.read c3)); done))
    ]

(* Some Simple Networks *)

let numbersInt o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
  let c4 = Csp.channel () in
    Csp.fork [
      prefixint 0 c3 c1;
      delta2int c1 c4 c2;
      succint c2 c3;
      (* we need some kind of a fn wrapper to make this pretty :/ *)
      (fun () -> (while true do (Csp.write o (Csp.read c4)); done))
    ]
      
let integrateInt i o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
  let c4 = Csp.channel () in
  let c5 = Csp.channel () in
    Csp.fork [
      (* we need some kind of a fn wrapper to make this pretty :/ *)
      (fun () -> (while true do (Csp.write c1 (Csp.read i)); done));
      plusint c1 c5 c2;
      delta2int c2 c3 c4;
      prefixint 0 c4 c5;
      (* we need some kind of a fn wrapper to make this pretty :/ *)
      (fun () -> (while true do (Csp.write o (Csp.read c3)); done))
    ]

let pairsInt i o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
  let c4 = Csp.channel () in
  let c5 = Csp.channel () in
    Csp.fork [
      (* we need some kind of a fn wrapper to make this pretty :/ *)
      (fun () -> (while true do (Csp.write c1 (Csp.read i)); done));
      delta2int c1 c2 c3;
      tailint c2 c4;
      plusint c3 c4 c5;
      (* we need some kind of a fn wrapper to make this pretty :/ *)
      (fun () -> (while true do (Csp.write o (Csp.read c5)); done))
    ]

(* Some Layered Networks *)

let fibonacciInt o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
  let c4 = Csp.channel () in
  let c5 = Csp.channel () in
    Csp.fork [
      prefixint 1 c5 c1 ;
      prefixint 0 c1 c2 ;
      delta2int c2 c3 c4;
      pairsInt c4 c5;
      (* we need some kind of a fn wrapper to make this pretty :/ *)
      (fun () -> (while true do (Csp.write o (Csp.read c3)); done))
    ]

let squaresInt o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
    Csp.fork [
      numbersInt c1;
      integrateInt c1 c2;      
      pairsInt c2 c3;
      (* we need some kind of a fn wrapper to make this pretty :/ *)
      (fun () -> (while true do (Csp.write o (Csp.read c3)); done))
    ]

    
