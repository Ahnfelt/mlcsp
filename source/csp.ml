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


module Csp = struct


    module type CHANNEL = sig

        type ('a, 'c) t
        
        exception PoisonException
        
        val channel : unit -> ('a, [`Read | `Write | `Poison]) t
        
        val read : ('a, [> `Read]) t -> 'a
        val write : ('a, [> `Write]) t -> 'a -> unit
        val poison : ('a, [> `Poison]) t -> unit

        val poison_only : ('a, [> `Poison]) t -> ('a, [`Poison]) t
        val write_only : ('a, [> `Write]) t -> ('a, [`Write]) t
        val write_poison_only : ('a, [> `Write | `Poison]) t -> ('a, [`Write | `Poison]) t
        val read_only : ('a, [> `Read]) t -> ('a, [`Read]) t
        val read_poison_only : ('a, [> `Read | `Poison]) t -> ('a, [`Read | `Poison]) t
        val read_write_only : ('a, [> `Read | `Write]) t -> ('a, [`Read | `Write]) t
        
    end


    module type PROCESS = sig

        val spawn : int -> (int -> unit) -> unit

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


    module Channel : CHANNEL = struct

        type 'a slot = Empty | HasValue of 'a | Poison

        type ('a, 'c) t = 'a slot ref * Mutex.t * Condition.t * Condition.t * Condition.t

        exception PoisonException

        let channel () = (ref Empty, Mutex.create (), Condition.create (), Condition.create (), Condition.create ())

        let with_mutex m f = (
            Mutex.lock m; 
            let v = try f m with e -> Mutex.unlock m; raise e in 
            Mutex.unlock m;
            v
            )

        let read (r, m, cr, cw, cd) = 
            let rec aux m = (
                match !r with
                | Empty -> (
                    Condition.wait cr m;
                    aux m
                    )
                | HasValue v -> (
                    r := Empty;
                    Condition.signal cd;
                    Condition.signal cw;
                    v
                    )
                | Poison -> raise PoisonException
                )
            in with_mutex m aux

        let write (r, m, cr, cw, cd) v = 
            let rec aux m = (
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
                    aux m
                    )
                | Poison -> raise PoisonException
                )
            in with_mutex m aux

        let poison (r, m, cr, cw, cd) = with_mutex m (fun m ->
            r := Poison;
            Condition.broadcast cr;
            Condition.broadcast cw;
            Condition.broadcast cd
            )
            
        let poison_only c = c
        let write_only c = c
        let write_poison_only c = c
        let read_only c = c
        let read_poison_only c = c
        let read_write_only c = c

    end


    module Process : PROCESS = struct
        
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
                | Bad e -> raise e
            in (extract r1, extract r2)
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
            | [] -> ()
            in try aux fs with _ -> ()
            
        let generate_list n f = let rec aux i l = match i with
            | 0 -> l
            | i -> aux (i - 1) (f i :: l)
            in aux n []
            
        let spawn n f = fork (generate_list n (fun i -> fun () -> f i))
        
    end


    include Channel
    include Process

end

(*
    Proposal for alternation interface:
    Csp.prioritized [
        Csp.Read (c1, fun v -> v * v);
        Csp.Read (c2, fun v -> v + v);
        Csp.Write (c3, (fun () -> v), fun v -> 0);
        Csp.Wait (0.100, fun t -> 0);
    ]
    Csp.arbitrary [ ... ] - may not be too interesting to support.
    Csp.random [ ... ]
    Csp.fair [ ... ] - can this be done with that interface?
    In any case, random may be enough.
*)

(*
let _ = 
    let c = Csp.channel () in
    let (a, b, c, d, e, f, g, h) = Csp.parallel8
        (fun () -> Csp.read c)
        (fun () -> Csp.write c "r1"; "w1") 
        (fun () -> Csp.write c "r2"; "w2") 
        (fun () -> Csp.read c)
        (fun () -> Csp.read c)
        (fun () -> Csp.write c "r3"; "w3") 
        (fun () -> Csp.read c)
        (fun () -> Csp.write c "r4"; "w4") 
        in
    print_endline (a ^ b ^ c ^ d ^ e ^ f ^ g ^ h)
*)

(*
let _ = 
    let printer cin i =
        while true do print_endline (Csp.read cin ^ " from printer #" ^ string_of_int i) done in
    let generator cout () =
        Csp.write cout "1"; 
        Csp.write cout "2"; 
        Csp.write cout "3"; 
        Csp.poison cout; in
    let c = Csp.channel () in
        Csp.fork [
            (fun () -> Csp.spawn 10 (printer (Csp.read_only c)));
            generator (Csp.write_poison_only c);
        ]
*)
