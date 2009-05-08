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

(* let rec idint in1 out () =  *)

(* let rec succint in1 out () =  *)

let rec plusint in1 in2 out () =
    let (x, y) = Process.parallel
      (fun () -> Channel.read in1)
      (fun () -> Channel.read in2) in
      Channel.write out (x + y); plusint in1 in2 out ()

(* let rec delta2int in out1 out2 () =  *)

(* let rec prefixint n in out () =  *)

(* let rec tailint n in out () =  *)
