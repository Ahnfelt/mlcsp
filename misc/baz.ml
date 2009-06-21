(* Argumenter for poison *)
(* Hvad sker der hvis alternativ to er forgiftet, 
   men alternativ et er klar? Lige nu læser den vidst
   fra den klare, men det er måske ikke 
   hensigtsmæssigt. Det er også muligt at select kun
   skal kaste PoisonException hvis alle dens 
   valgmuligheder er poisoned. *)
(* Overvej conditional guards. *)

module type Csp = sig
    exception PoisonException
    type 'a channel
    type 'a guard
    val channel : unit -> 'a channel
    val poison : 'a channel -> unit
    val select : ('a guard) list -> 'a
    val read_guard : 'a channel -> ('a -> 'b) -> 'b guard
    val write_guard : 'a channel -> 'a -> ('a -> 'b) -> 'b guard
    val read : 'a channel -> 'a
    val write : 'a channel -> 'a -> unit
    val parallel : (unit -> unit) list -> unit
    val parallel_collect : (unit -> unit) list -> (unit -> 'a) -> 'a
end

(* Consider making a toolbox that has parallel_collect and derivatives *)

module Csp : Csp = struct

    exception PoisonException

    type 'a concrete_guard = {
        attempt: unit -> 'a option;
        check_poison: unit -> bool;
        subscribe: ('a concrete_guard) list -> ('a option) ref -> unit;
        unsubscribe: unit -> unit;
        }

    type 'a guard = Condition.t -> 'a concrete_guard

    type 'a channel_state
        = NobodyWaiting 
        | ReaderWaiting of (Condition.t * ('a -> unit)) list
        | WriterWaiting of (Condition.t * (unit -> 'a)) list
        | Poisoned

    type 'a channel = ('a channel_state) ref

    type ('a, 'b) either = Left of 'a | Right of 'b

    let global_mutex = Mutex.create ()

    (* Knuth/Fisher-Yates shuffle *)
    (* Random is probably NOT thread safe, call in a locked context *)
    let shuffle l =
        match l with [] | [_] -> l | _ ->
        let a = Array.of_list l in
        let rec loop i = if i <= 1 then () else 
            let n = i - 1 in
            let k = Random.int (n + 1) in
            let t = Array.get a k in
            Array.set a k (Array.get a n);
            Array.set a n t;
            loop n
        in loop (Array.length a); Array.to_list a

    let with_mutex m f = (
        Mutex.lock m; 
        let v = try f m with e -> Mutex.unlock m; raise e in 
        Mutex.unlock m;
        v)

    let channel () = ref NobodyWaiting

    let poison c = with_mutex global_mutex (fun _ -> 
        let f = fun (s, _) -> Condition.signal s in
        (match !c with
        | ReaderWaiting xs -> List.iter f xs 
        | WriterWaiting xs -> List.iter f xs
        | _ -> ());
        c := Poisoned)

    (* Must be called in a locked context *)
    let rec attempt_all l = match l with
        | [] -> None
        | (h::t) -> match h.attempt () with
            | Some v -> Some v
            | None -> attempt_all t

    (* Must be called in a locked context *)
    let rec check_poison_all l = match l with
        | [] -> false
        | (h::t) -> if h.check_poison () 
            then true else check_poison_all t

    (* Must be called in a locked context *)
    let subscribe_all l r = let rec loop k = match k with
        | [] -> ()
        | (h::t) -> h.subscribe l r; loop t
        in loop l

    (* Must be called in a locked context *)
    let rec unsubscribe_all l = match l with
        | [] -> ()
        | (h::t) -> (h.unsubscribe (); unsubscribe_all t)
        
    (* Uses the global lock *)
    let select l = with_mutex global_mutex (fun m ->
        let s = Condition.create () in
        let l = List.map (fun x -> x s) (shuffle l) in
        match attempt_all l with
        | Some v -> v
        | None -> let r = ref None in (subscribe_all l r;
            let rec loop () = 
                if check_poison_all l 
                then (unsubscribe_all l; raise PoisonException) 
                else match !r with
                | Some v -> v
                | None -> (Condition.wait s m; loop ())
            in loop ()))

    let transmit l r f v s =
        r := Some (f v);
        unsubscribe_all l;
        Condition.signal s

    (* Methods must be called in a locked context *)
    let read_guard c f s = {
        attempt = (fun () -> match !c with
            | WriterWaiting ((_, x)::_) -> Some (f (x ()))
            | _ -> None
        );
        check_poison = (fun () -> match !c with
            | Poisoned -> true
            | _ -> false
        );
        subscribe = (fun l r ->
            let g = (s, fun v -> transmit l r f v s) 
            in match !c with
            | NobodyWaiting -> c := ReaderWaiting [g]
            | ReaderWaiting gs -> c := ReaderWaiting (gs @ [g])
            | Poisoned -> () (* Ignore *)
            | WriterWaiting _ -> () (* Shouldn't ever happen *)
        );
        unsubscribe = (fun () -> match !c with
            | ReaderWaiting gs -> 
                (match List.filter (fun (i, _) -> i <> s) gs with
                | [] -> c := NobodyWaiting
                | gs -> c := ReaderWaiting gs)
            | _ -> ()
        );
        }

    (* Methods must be called in a locked context *)
    let write_guard c v f s = {
        attempt = (fun () -> match !c with
            | ReaderWaiting ((_, x)::_) -> (x v; Some (f v))
            | _ -> None
        );
        check_poison = (fun () -> match !c with
            | Poisoned -> true
            | _ -> false
        );
        subscribe = (fun l r -> 
            let g = (s, fun () -> transmit l r f v s; v) 
            in match !c with
            | NobodyWaiting -> c := WriterWaiting [g]
            | WriterWaiting gs -> c := WriterWaiting (gs @ [g])
            | Poisoned -> () (* Ignore *)
            | ReaderWaiting _ -> () (* Shouldn't ever happen *)
        );
        unsubscribe = (fun () -> match !c with
            | WriterWaiting gs -> 
                (match List.filter (fun (i, _) -> i <> s) gs with
                | [] -> c := NobodyWaiting
                | gs -> c := WriterWaiting gs)
            | _ -> ()
        );
        }

    let read c = select [read_guard c (fun x -> x)]
    let write c v = select [write_guard c v (fun _ -> ())]

    (* Ignores exceptions (for example, PoisonException) *)
    let parallel fs = let fs = shuffle fs in
        let ts = List.map (fun f -> Thread.create (fun () -> 
            try f () with PoisonException -> ()) ()) fs
        in List.iter Thread.join ts

    let parallel_collect fs f =
        let r = ref (Left PoisonException) in
        let f () = try r := Right (f ()) with e -> r := Left e in
        parallel (f::fs);
        match !r with
        | Left e -> raise e
        | Right v -> v

end

(*
let _ = 
    let c1 = channel () in
    let c2 = channel () in
    let c3 = channel () in
    parallel [
    (let rec loop () = let v = select [
        read_guard c1;
        read_guard c2;
    ] in print_endline v; loop () in loop);
    (let rec loop () = print_endline (read c3); loop () in loop);
    (let rec loop () = write c2 "2"; loop () in loop);
    (let rec loop () = select [write_guard c1 "1"; write_guard c3 "3"]; loop () in loop);
    ];
*)

let id x = x

(*
let _ = 
    let c1 = channel () in
    let c2 = channel () in
    let c3 = channel () in
    parallel [

        (let rec loop () = let v = select [
            read_guard c1 id;
            read_guard c2 id;
            write_guard c3 "3" (fun _ -> "write 3");
        ] in print_endline v; loop () in loop);

        (let rec loop () = let v = select [
            read_guard c3 id;
            write_guard c2 "2" (fun _ -> "write 2");
            write_guard c1 "1" (fun _ -> "write 1");
        ] in print_endline v; loop () in loop);

    ];
*)

let _ = 
    let c1 = Csp.channel () in
    let c2 = Csp.channel () in
    let c3 = Csp.channel () in
    Csp.parallel [

        (let rec loop () = let v = Csp.select [
            Csp.read_guard c1 id;
            Csp.read_guard c2 id;
            Csp.write_guard c3 "3" (fun _ -> "write 3 a");
        ] in print_endline v; loop () in loop);

        (let rec loop () = let v = Csp.select [
            Csp.write_guard c3 "3" (fun _ -> "write 3 b");
            Csp.read_guard c2 id;
            Csp.write_guard c1 "1" (fun _ -> "write 1 b");
        ] in print_endline v; loop () in loop);

        (let rec loop () = let v = Csp.select [
            Csp.read_guard c3 id;
            Csp.write_guard c2 "2" (fun _ -> "write 2 c");
            Csp.write_guard c1 "1" (fun _ -> "write 1 c");
        ] in print_endline v; loop () in loop);

        (let rec loop () = Csp.write c2 "2"; print_endline "write 2 d"; loop () in loop);

        (fun () -> Thread.delay 5.0; Csp.poison c2);
    ]


(* lego stuff *)
let operator f a b () = 
    let a' = Csp.channel () in
    let b' = Csp.channel () in
    Csp.parallel_collect [
        (fun () -> Csp.write a' (Csp.read a));
        (fun () -> Csp.write b' (Csp.read b));
    ]   (fun () -> f (Csp.read a') (Csp.read b'))

let plus = operator ( + )
let minus = operator ( - )
let times = operator ( * )
let over = operator ( / )
(* let pair = operator (fun a b -> (a, b)) (* does not type check! *) *)

let pair a b () = 
    let a' = Csp.channel () in
    let b' = Csp.channel () in
    Csp.parallel_collect [
        (fun () -> Csp.write a' (Csp.read a));
        (fun () -> Csp.write b' (Csp.read b));
    ]   (fun () -> (Csp.read a', Csp.read b'))

