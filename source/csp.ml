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

    type ('a, 'c) t
    
    exception PoisonException
    
    val create : unit -> ('a, [`Read | `Write | `Poison]) t
    
    val read : ('a, [> `Read]) t -> 'a
    val write : ('a, [> `Write]) t -> 'a -> unit
    
    val poison : ('a, [> `Poison]) t -> unit

    val read_only : ('a, [> `Read]) t -> ('a, [`Read]) t
    val write_only : ('a, [> `Write]) t -> ('a, [`Write]) t
    val read_poison_only : ('a, [> `Read | `Poison]) t -> ('a, [`Read | `Poison]) t
    val write_poison_only : ('a, [> `Write | `Poison]) t -> ('a, [`Write | `Poison]) t
    
end

module type Process = sig

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

    type 'a slot = Empty | HasValue of 'a | Poison

    type ('a, 'c) t = 'a slot ref * Mutex.t * Condition.t * Condition.t * Condition.t
    
    exception PoisonException

    let create () = (ref Empty, Mutex.create (), Condition.create (), Condition.create (), Condition.create ())

    let with_mutex m f = (
        Mutex.lock m; 
        let v = try f () with e -> Mutex.unlock m; raise e in 
        Mutex.unlock m;
        v
        )

    let read (r, m, cr, cw, cd) = 
        let rec aux () = (
            match !r with
            | Empty -> (
                Condition.wait cr m;
                aux ()
                )
            | HasValue v -> (
                r := Empty;
                Condition.signal cd;
                Condition.signal cw;
                v
                )
            | Poison -> raise PoisonException
            ) in
        with_mutex m aux

    let write (r, m, cr, cw, cd) v = 
        let rec aux () = (
            match !r with
            | Empty -> (
                r := HasValue v;
                Condition.signal cr;
                Condition.wait cd m;
                (if !r = Poison then raise PoisonException);
                ()
                )
            | HasValue _ -> (
                Condition.wait cw m;
                aux ()
                )
            | Poison -> raise PoisonException
            ) in
        with_mutex m aux

    let poison (r, m, cr, cw, cd) = with_mutex m (fun () ->
        r := Poison;
        Condition.broadcast cr;
        Condition.broadcast cw;
        Condition.broadcast cd
        )
        
    let read_only c = c
    let write_only c = c
    let read_poison_only c = c
    let write_poison_only c = c

end

module Process : Process = struct
    
    type 'a result = Good of 'a | Bad of exn
    
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
    let (a, b, c, d, e, f, g, h) = Process.parallel8
        (fun () -> Channel.read c)
        (fun () -> Channel.write c "r1"; "w1") 
        (fun () -> Channel.write c "r2"; "w2") 
        (fun () -> Channel.read c)
        (fun () -> Channel.read c)
        (fun () -> Channel.write c "r3"; "w3") 
        (fun () -> Channel.read c)
        (fun () -> Channel.write c "r4"; "w4") 
        in
    print_endline (a ^ b ^ c ^ d ^ e ^ f ^ g ^ h)

let _ = 
    let printer cin () =
        while true do print_endline (Channel.read cin) done in
    let generator cout () =
        Channel.write cout "1"; 
        Channel.write cout "2"; 
        Channel.write cout "3"; 
        Channel.poison cout; in
    let c = Channel.create () in
        Process.fork [
            printer (Channel.read_only c);
            generator (Channel.write_poison_only c);
        ]

