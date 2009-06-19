let global_mutex = Mutex.create ()
let global_condition = Condition.create ()

exception CspException

type 'a channel_state
    = NobodyWaiting 
    | ReaderWaiting of ('a -> unit)
    | WriterWaiting of (unit -> 'a)

type 'a channel = ('a channel_state) ref

type 'a guard = {
    attempt: unit -> 'a option;
    subscribe: ('a guard) list -> ('a option) ref -> unit;
    unsubscribe: unit -> unit;
    }

let channel () = ref NobodyWaiting

(* Must be called in a locked context *)
let rec attempt l = match l with
    | [] -> None
    | (h::t) -> match h.attempt () with
        | Some v -> Some v
        | None -> attempt t

(* Must be called in a locked context *)
let subscribe l r = let rec loop k = match k with
    | [] -> ()
    | (h::t) -> h.subscribe l r; loop t
    in loop l

(* Uses the global lock *)
let select l =
    Mutex.lock global_mutex;
    match attempt l with
    | Some v -> (Mutex.unlock global_mutex; v)
    | None -> let r = ref None in (subscribe l r;
        let rec loop () = match !r with
        | Some v -> (Mutex.unlock global_mutex; v)
        | None -> (Condition.wait global_condition global_mutex; loop ())
        in loop ())

(* Methods must be called in a locked context *)
let read_guard c f = {
    attempt = (fun () -> match !c with
        | WriterWaiting x -> Some (f (x ()))
        | _ -> None
    );
    subscribe = (fun l r -> match !c with
        | NobodyWaiting -> c := ReaderWaiting (fun v ->
            r := Some (f v);
            (let rec loop l = match l with
            | [] -> ()
            | (h::t) -> h.unsubscribe (); loop t
            in loop l);
            Condition.broadcast global_condition)
        | _ -> raise CspException
    );
    unsubscribe = (fun () -> c := NobodyWaiting);
    }

(* Methods must be called in a locked context *)
let write_guard c v f = {
    attempt = (fun () -> match !c with
        | ReaderWaiting x -> (x v; Some (f v))
        | _ -> None
    );
    subscribe = (fun l r -> match !c with
        | NobodyWaiting -> c := WriterWaiting (fun () ->
            r := Some (f v);
            (let rec loop l = match l with
            | [] -> ()
            | (h::t) -> h.unsubscribe (); loop t
            in loop l);
            Condition.broadcast global_condition;
            v)
        | _ -> raise CspException
    );
    unsubscribe = (fun () -> c := NobodyWaiting);
    }


let read c = select [read_guard c (fun x -> x)]
let write c v = select [write_guard c v (fun _ -> ())]

let parallel fs =
    let ts = List.map (fun f -> Thread.create f ()) fs in
    List.iter Thread.join ts

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

