module type CSP = sig

    type 'a channel
    (** Represents a channel accepting messages of type 'a *)
    
    type 'a guard
    (** Represents a guard resulting in a value of type 'a *)
    
    type process = unit -> unit
    (** A shorthand for the process type which really is just a function whose
        only interesting property is its side effects. *)

    val channel : unit -> 'a channel
    (** Creates a new channel *)
    
    val spawn : int -> process -> process
    (** Runs multiple processes in parallel and returns when they are all done *)

    val parallel : process list -> process
    (** Runs multiple processes in parallel and returns when they are all done *)
    
    val prioritized : ('a guard) list -> unit -> 'a
    (** Selects one of the guards in the list; if multiple guards are ready, 
        the one with the smallest index in the list is chosen *)

    val read_guard : 'a channel -> ('a -> 'b) -> 'b guard
    (** Creates a guard that will read from a channel and apply a function. *)
    
    val write_guard : 'a channel -> 'a -> ('a -> 'b) -> 'b guard
    (** Creates a guard that will write to a channel and apply a function. *)
    
    val read : 'a channel -> 'a
    (** Shortcut for a select with exactly one read guard. *)

    val write : 'a channel -> 'a -> unit
    (** Shortcut for a select with exactly one write guard. *)

end

module type HAPPENING = sig

    type t
    
    val create : unit -> t
    
    val register : t -> (Mutex.t * Condition.t) -> unit

    val unregister : t -> (Mutex.t * Condition.t) -> unit
    
    val signal : t -> unit

end

module Happening : HAPPENING = struct
    
    type t = ((Mutex.t * Condition.t) list) ref
    
    let create () = ref []
    
    let register h c = h := (!h) @ [c]
    
    let unregister h c = let rec aux l r = match r with
        | [] -> List.rev l
        | (x::xs) -> if x = c then List.rev l @ xs else aux (x::l) xs
        in h := aux [] (!h)

    let signal h = match !h with
        | ((m, c)::_) -> Mutex.lock m; Condition.signal c; Mutex.unlock m
        | [] -> ()

end

module Csp : CSP = struct
    
    type 'a channel = {
        mutex: Mutex.t;
        mutable readers: Happening.t;
        mutable writers: Happening.t;
        mutable value: 'a option;
        }

    type 'a guard = {
        acquire: Mutex.t -> Condition.t -> unit;
        release: Mutex.t -> Condition.t -> unit;
        attempt: unit -> 'a option;
        }

    type process = unit -> unit

    let parallel fs () = 
        let ts = List.map (fun f -> Thread.create f ()) fs in
        List.iter Thread.join ts

    let spawn n f () =
        let rec aux n l = if n = 0 then l else aux (n - 1) (f::l) in
        parallel (aux n []) ()

    let channel () = {
        mutex = Mutex.create ();
        readers = Happening.create ();
        writers = Happening.create ();
        value = None;
        }

    let with_mutex m f = (
        Mutex.lock m;
        let v = try f m with e -> Mutex.unlock m; raise e in 
        Mutex.unlock m;
        v)

    let rec iterate l = match l with
        | [] -> None
        | (h::t) -> match h.attempt () with
            | Some v -> Some v
            | None -> iterate t

    let prioritized l () =
        let rec aux c m = match iterate l with
            | Some v -> v
            | None -> Condition.wait c m; aux c m
        in with_mutex (Mutex.create ()) (fun m -> 
            let c = Condition.create () in
            List.iter (fun a -> a.acquire m c) l;
            let v = aux c m in
            List.iter (fun a -> a.release m c) l;
            v)

    (* other selection strategies: round_robin, random, same *)

    let guard_match a f g () = 
        let option_map f v = match v with
            | Some v -> Some (f v)
            | None -> None
        in option_map f (with_mutex a.mutex (fun _ -> g a.value))

    let read_guard a f = {
        acquire = (fun m c -> with_mutex a.mutex (fun _ -> Happening.register a.readers (m, c)));
        release = (fun m c -> with_mutex a.mutex (fun _ -> Happening.unregister a.readers (m, c)));
        attempt = guard_match a f (fun x -> match x with
            | Some v -> 
                a.value <- None;
                Happening.signal a.writers;
                Some v
            | None -> None)
        }

    let write_guard a v f = {
        acquire = (fun m c -> with_mutex a.mutex (fun _ -> Happening.register a.writers (m, c)));
        release = (fun m c -> with_mutex a.mutex (fun _ -> Happening.unregister a.writers (m, c)));
        attempt = guard_match a f (fun x -> match x with
            | Some _ -> None
            | None -> 
                a.value <- Some v;
                Happening.signal a.readers;
                Some v)
        }
        
    (* other guard types: time_guard *)

    let read a = prioritized [read_guard a (fun v -> v)] ()
    let write a v = prioritized [write_guard a v (fun _ -> ())] ()

end

exception Foo

let rec forever f () = f (); forever f ()

let _ = 
    let v = ref 200000 in
    let c = Csp.channel () in
    let c = Csp.channel () in
    Csp.parallel [
        forever (fun () -> print_string (Csp.read c));
        forever (fun () -> Csp.write c ".");
    ] ()
    
