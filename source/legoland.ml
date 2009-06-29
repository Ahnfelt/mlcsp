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

open Big_int
open Csp

let bi x = big_int_of_int x
let (++) x y = add_big_int x y

let rec terminator i () =
  Csp.read i;
  terminator i ()

let rec printer i () =
  let x = Csp.read i in
    (* print_endline(string_of_int x); *)
    print_endline(string_of_big_int x);
    printer i ()

let rec idint i o () =
  Csp.write o (Csp.read i);
  idint i o ()

let rec succint i o () =
  Csp.write o (Csp.read i ++ bi 1);
  succint i o ()

let rec plusint i1 i2 o () =
  let i1' = Csp.channel () in
  let i2' = Csp.channel () in
    Csp.parallel [
      (fun () -> Csp.write i1' (Csp.read i1));
      (fun () -> Csp.write i2' (Csp.read i2));
      (fun () -> Csp.write o (Csp.read i1' ++ Csp.read i2'));
    ];
    plusint i1 i2 o ()

let rec delta2int i o1 o2 () =
  let x = Csp.read i in
  Csp.parallel[
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
      prefixint (bi 0) c3 c1;
      delta2int c1 o c2;
      succint c2 c3;
    ]
      
let integrateInt i o () =
  let c1 = Csp.channel () in
  let c2 = Csp.channel () in
  let c3 = Csp.channel () in
    Csp.parallel [
      plusint i c3 c1;
      delta2int c1 o c2;
      prefixint (bi 0) c2 c3;
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
      prefixint (bi 1) c4 c1 ;
      prefixint (bi 0) c1 c2 ;
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
