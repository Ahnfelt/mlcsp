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

(* Process *)

(* Channels *)

module type Channel = sig

    type 't t
    
    val create: unit -> 't t
    
    val read: 't t -> 't

    val write: 't t -> 't -> unit
    
end

module type Process = sig

    type 't t
    
    val run: (unit -> 'b) -> 'b

end

module Channel = struct

    type 't slot = Empty | HasValue of 't

    type 't t = 't slot ref * Mutex.t * Condition.t

    let create () = (ref Empty, Mutex.create (), Condition.create ())

    let withMutex m f = (
        Mutex.lock m; 
        let v = try f () with e -> Mutex.unlock m; raise e in 
        Mutex.unlock m; 
        v
        )

    let rec read (r, m, c) = withMutex m (fun () ->
        match !r with
        | Empty -> (
            Condition.wait c m;
            read (r, m, c)
            )
        | HasValue v -> (
            Condition.signal c;
            v
            )
        )

    let rec write (r, m, c) n = withMutex m (fun () ->
        match !r with
        | Empty -> (
            r := HasValue n;
            Condition.signal c;
            Condition.wait c m
            )
        | HasValue v -> (
            Condition.wait c m;
            write (r, m, c) n
            );
        ()
        )

end

module Process = struct
    
    type 't t = 't option ref * Thread.t
    
    type 't result = Good of 't | Bad of exn
    
    exception Exception
    
    let run f = 
        let r = ref (Bad Exception) in 
        let t = Thread.create (
            fun () -> r := 
                try Good (f ()) with
                e -> Bad e
        ) () in (
        Thread.join t;
        match !r with
        | Good v -> v
        | Bad e -> raise e
        )
        
    let parallel f1 f2 = 
        let spawn f r = Thread.create (
            fun () -> r := 
                try Good (f ()) with
                e -> Bad e
        ) () in (
        let r1 = ref (Bad Exception) in 
        let r2 = ref (Bad Exception) in 
        let t1 = spawn f1 r1 in
        let t2 = spawn f2 r2 in
        Thread.join t1;
        Thread.join t2;
        let extract r = match !r with
            | Good v -> v
            | Bad e -> raise e in
        (extract r1, extract r2)
        )
        
end

(* let _ = print_endline (Process.run (fun () -> "foo")) *)

let _ = 
    let c = Channel.create () in
    let (x, y) = Process.parallel
        (fun () -> Channel.write c "bar"; "foo")
        (fun () -> Channel.read c) in
    print_endline (x ^ y)


