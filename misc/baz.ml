let global_mutex = Mutex.create ()
let global_condition = Condition.create ()

(* Argumenter for poison *)

exception CspException

type 'a channel_state
    = NobodyWaiting 
    | ReaderWaiting of (int * ('a -> unit)) list
    | WriterWaiting of (int * (unit -> 'a)) list

type 'a channel = ('a channel_state) ref

type 'a guard = {
    attempt: unit -> 'a option;
    subscribe: ('a guard) list -> ('a option) ref -> unit;
    unsubscribe: int -> unit;
    }

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

let channel () = ref NobodyWaiting

(* Must be called in a locked context *)
let rec attempt_all l = match l with
    | [] -> None
    | (h::t) -> match h.attempt () with
        | Some v -> Some v
        | None -> attempt_all t

(* Must be called in a locked context *)
let subscribe_all l r = let rec loop k = match k with
    | [] -> ()
    | (h::t) -> h.subscribe l r; loop t
    in loop l

(* Uses the global lock *)
let select l =
    Mutex.lock global_mutex;
    let l = shuffle l in
    match attempt_all l with
    | Some v -> (Mutex.unlock global_mutex; v)
    | None -> let r = ref None in (subscribe_all l r;
        let rec loop () = match !r with
        | Some v -> (Mutex.unlock global_mutex; v)
        | None -> (Condition.wait global_condition global_mutex; loop ())
        in loop ())

let transmit l r f v s =
    r := Some (f v);
    (let rec loop l = match l with
    | [] -> ()
    | (h::t) -> h.unsubscribe s; loop t
    in loop l);
    Condition.broadcast global_condition

(* Methods must be called in a locked context *)
let read_guard c f = {
    attempt = (fun () -> match !c with
        | WriterWaiting ((_, x)::_) -> Some (f (x ()))
        | _ -> None
    );
    subscribe = (fun l r ->
        let s = Thread.id (Thread.self ()) in
        let g = (s, fun v -> transmit l r f v s) 
        in match !c with
        | NobodyWaiting -> c := ReaderWaiting [g]
        | ReaderWaiting gs -> c := ReaderWaiting (gs @ [g])
        | _ -> raise CspException (* Shouldn't ever happen *)
    );
    unsubscribe = (fun s -> match !c with
        | ReaderWaiting gs -> 
            match List.filter (fun (i, _) -> i <> s) gs with
            | [] -> c := NobodyWaiting
            | gs -> c := ReaderWaiting gs
        | _ -> raise CspException (* Shouldn't ever happen *)
    );
    }

(* Methods must be called in a locked context *)
let write_guard c v f = {
    attempt = (fun () -> match !c with
        | ReaderWaiting ((_, x)::_) -> (x v; Some (f v))
        | _ -> None
    );
    subscribe = (fun l r -> 
        let s = Thread.id (Thread.self ()) in
        let g = (s, fun () -> transmit l r f v s; v) 
        in match !c with
        | NobodyWaiting -> c := WriterWaiting [g]
        | WriterWaiting gs -> c := WriterWaiting (gs @ [g])
        | _ -> raise CspException (* Shouldn't ever happen *)
    );
    unsubscribe = (fun s -> match !c with
        | WriterWaiting gs -> 
            match List.filter (fun (i, _) -> i <> s) gs with
            | [] -> c := NobodyWaiting
            | gs -> c := WriterWaiting gs
        | _ -> raise CspException (* Shouldn't ever happen *)
    );
    }

let read c = select [read_guard c (fun x -> x)]
let write c v = select [write_guard c v (fun _ -> ())]

(* TODO: Don't ignore exceptions from other threads *)
(* Consider using the original parallel/fork construct *)
let parallel fs =
    match fs with
    | [] -> ()
    | [h] -> h ()
    | (h::t) -> 
        let ts = List.map (fun f -> Thread.create f ()) t
        in h (); List.iter Thread.join ts

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
    let c1 = channel () in
    let c2 = channel () in
    let c3 = channel () in
    parallel [

        (let rec loop () = let v = select [
            read_guard c1 id;
            read_guard c2 id;
            write_guard c3 "3" (fun _ -> "write 3 a");
        ] in print_endline v; loop () in loop);

        (let rec loop () = let v = select [
            write_guard c3 "3" (fun _ -> "write 3 b");
            read_guard c2 id;
            write_guard c1 "1" (fun _ -> "write 1 b");
        ] in print_endline v; loop () in loop);

        (let rec loop () = let v = select [
            read_guard c3 id;
            write_guard c2 "2" (fun _ -> "write 2 c");
            write_guard c1 "1" (fun _ -> "write 1 c");
        ] in print_endline v; loop () in loop);

        (let rec loop () = write c2 "2"; print_endline "write 2 d"; loop () in loop);
    ];

