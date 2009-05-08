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

(* $Id: csp.ml,v 1.0 2009/05/05 09:00:00 gentauro Exp $ *)

module type Channel = sig

    type 't t
    
    val create : unit -> 't t
    
    val read : 't t -> 't

    val write : 't t -> 't -> unit
    
    val poison : 't t -> unit

    exception PoisonException
    
end

module type Process = sig

    type 't t

    val fork : (unit -> unit) list -> unit

    val parallel : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b
    
    val parallel3 : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> 'a * 'b * 'c
    val parallel4 : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> (unit -> 'd) -> 'a * 'b * 'c * 'd
    val parallel5 : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> (unit -> 'd) -> (unit -> 'e) -> 'a * 'b * 'c * 'd * 'e
    val parallel6 : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> (unit -> 'd) -> (unit -> 'e) -> (unit -> 'f) -> 'a * 'b * 'c * 'd * 'e * 'f
    val parallel7 : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> (unit -> 'd) -> (unit -> 'e) -> (unit -> 'f) -> (unit -> 'g) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g
    val parallel8 : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> (unit -> 'd) -> (unit -> 'e) -> (unit -> 'f) -> (unit -> 'g) -> (unit -> 'h) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h
    val parallel9 : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> (unit -> 'd) -> (unit -> 'e) -> (unit -> 'f) -> (unit -> 'g) -> (unit -> 'h) -> (unit -> 'i) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i

end

module Channel : Channel = struct

    type 't slot = Empty | HasValue of 't | Poison

    type 't t = 't slot ref * Mutex.t * Condition.t * Condition.t
    
    exception PoisonException

    let create () = (ref Empty, Mutex.create (), Condition.create (), Condition.create ())

    let withMutex m f = (
        Mutex.lock m; 
        let v = try f () with e -> Mutex.unlock m; raise e in 
        Mutex.unlock m;
        v
        )

    let read (r, m, cr, cw) = 
        let rec aux () = (
            match !r with
            | Empty -> (
                Condition.wait cr m;
                aux ()
                )
            | HasValue v -> (
                Condition.signal cw;
                v
                )
            | Poison -> raise PoisonException
            ) in
        withMutex m aux

    let rec write (r, m, cr, cw) n = 
        let rec aux () = (
            match !r with
            | Empty -> (
                r := HasValue n;
                Condition.signal cr;
                Condition.wait cw m;
                (if !r = Poison then raise PoisonException);
                ()
                )
            | HasValue v -> (
                Condition.wait cr m;
                aux ()
                )
            | Poison -> raise PoisonException
            ) in
        withMutex m aux

    let rec poison (r, m, cr, cw) = withMutex m (fun () ->
        r := Poison;
        Condition.signal cr;
        Condition.signal cw
        )

end

module Process : Process = struct
    
    type 't t = 't option ref * Thread.t
    
    type 't result = Good of 't | Bad of exn
    
    exception NeverException

    let parallel f1 f2 = 
        let attempt f r = r := try Good (f ()) with e -> Bad e in
        let r1 = ref (Bad NeverException) in 
        let r2 = ref (Bad NeverException) in 
        let t1 = Thread.create (fun () -> attempt f1 r1) () in (
        attempt f2 r2;
        Thread.join t1;
        let extract r = match !r with
            | Good v -> v
            | Bad e -> raise e in
        (extract r1, extract r2)
        )
        
    let parallel3 f1 f2 f3 = let (v1, (v2, v3)) = parallel f1 (fun () -> parallel f2 f3) in (v1, v2, v3)
    let parallel4 f1 f2 f3 f4 = let (v1, (v2, v3, v4)) = parallel f1 (fun () -> parallel3 f2 f3 f4) in (v1, v2, v3, v4)
    let parallel5 f1 f2 f3 f4 f5 = let (v1, (v2, v3, v4, v5)) = parallel f1 (fun () -> parallel4 f2 f3 f4 f5) in (v1, v2, v3, v4, v5)
    let parallel6 f1 f2 f3 f4 f5 f6 = let (v1, (v2, v3, v4, v5, v6)) = parallel f1 (fun () -> parallel5 f2 f3 f4 f5 f6) in (v1, v2, v3, v4, v5, v6)
    let parallel7 f1 f2 f3 f4 f5 f6 f7 = let (v1, (v2, v3, v4, v5, v6, v7)) = parallel f1 (fun () -> parallel6 f2 f3 f4 f5 f6 f7) in (v1, v2, v3, v4, v5, v6, v7)
    let parallel8 f1 f2 f3 f4 f5 f6 f7 f8 = let (v1, (v2, v3, v4, v5, v6, v7, v8)) = parallel f1 (fun () -> parallel7 f2 f3 f4 f5 f6 f7 f8) in (v1, v2, v3, v4, v5, v6, v7, v8)
    let parallel9 f1 f2 f3 f4 f5 f6 f7 f8 f9 = let (v1, (v2, v3, v4, v5, v6, v7, v8, v9)) = parallel f1 (fun () -> parallel8 f2 f3 f4 f5 f6 f7 f8 f9) in (v1, v2, v3, v4, v5, v6, v7, v8, v9)
 
    let fork fs = let rec aux fs = match fs with
        | f::fs -> ignore (parallel f (fun () -> aux fs))
        | [] -> () in
        try aux fs with _ -> ()
        
end


let _ = 
    let c = Channel.create () in
    let (x, y) = Process.parallel
        (fun () -> Channel.read c)
        (fun () -> Channel.write c "foo"; "bar") in
    print_endline (x ^ y)
