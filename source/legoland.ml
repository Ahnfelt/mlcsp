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

let rec idint inp out () =
  Csp.write out (Csp.read inp);
  idint inp out ()

let rec succint inp out () =
  Csp.write out ((Csp.read inp) + 1);
  succint inp out ()

let rec plusint inp1 inp2 out () =
  let (x, y) = Csp.parallel
    (fun () -> Csp.read inp1)
    (fun () -> Csp.read inp2) in
    Csp.write out (x + y);
    plusint inp1 inp2 out ()
      
let rec delta2int inp out1 out2 () =
  let x = Csp.read inp in
  let (y,z) = Csp.parallel
    (fun () -> Csp.write out1 (x))
    (fun () -> Csp.write out2 (x)) in
  delta2int inp out1 out2 ()

let rec prefixint n inp out () =
  Csp.write out (Csp.read (n));
  idint inp out ()

let rec tailint inp out () =
  Csp.read inp; (* we drop first number *)
  idint inp out ()

