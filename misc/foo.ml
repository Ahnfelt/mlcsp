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
    
    val forever : process -> process
    (** Returns a new process that repeats Repeats a process forever *)
    
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
    (** Reads from a channel (it will block until the channel is ready). *)

    val write : 'a channel -> 'a -> unit
    (** Writes to a channel (it will block until the channel is ready). *)

    val identity : 'a -> 'a
    (** Returns the value that is passed to it *)
    
    val ignore : 'a -> unit
    (** Ignores the value that is passed to it *)
    
end

module Csp : CSP = struct

    type 'a channel = {
        mutex: Mutex.t;
        mutable readers: (Mutex.t * Condition.t) list;
        mutable writers: (Mutex.t * Condition.t) list;
        mutable value: 'a option;
        }

    type 'a guard = {
        acquire: Mutex.t -> Condition.t -> unit;
        release: Mutex.t -> Condition.t -> unit;
        attempt: unit -> 'a option;
        }

    type process = unit -> unit

    let rec forever f () = f (); forever f ()

    let identity v = v
    let ignore _ = ()

    let parallel fs () = 
        let ts = List.map (fun f -> Thread.create f ()) fs in
        List.iter Thread.join ts

    let spawn n f () =
        let rec aux n l = if n = 0 then l else aux (n - 1) (f::l) in
        parallel (aux n []) ()

    let channel () = {
        mutex = Mutex.create ();
        readers = [];
        writers = [];
        value = None;
        }

    let with_mutex m f = (
        Mutex.lock m;
        let v = try f m with e -> Mutex.unlock m; raise e in 
        Mutex.unlock m;
        v)

    let option_map f v = match v with
        | Some v -> Some (f v)
        | None -> None

    let rec iterate l = match l with
        | [] -> None
        | (h::t) -> match h.attempt () with
            | Some v -> Some v
            | None -> iterate t

    let prioritized l () =
        let rec aux c m = match iterate l with
            | Some v -> v
            | None -> Condition.wait c m; aux c m in 
        let m = Mutex.create () in
        let c = Condition.create () in
        List.iter (fun a -> a.acquire m c) l;
        let v = with_mutex m (aux c) in
        List.iter (fun a -> a.release m c) l;
        v

    (* other selection strategies: round_robin, random, same *)

    let read_guard a f = {
        acquire = (fun m c -> with_mutex a.mutex (fun _ ->
            a.readers <- (m, c) :: a.readers));
        release = (fun m c -> with_mutex a.mutex (fun _ ->
            a.readers <- List.filter ((<>) (m, c)) a.readers));
        attempt = (fun () -> option_map f
            (with_mutex a.mutex (fun _ -> match a.value with
                | Some v -> 
                    a.value <- None;
                    (match a.writers with
                        | ((m, c)::t) -> with_mutex m (fun _ -> Condition.signal c)
                        | [] -> ());
                    Some v
                | None -> None)));
        }

    let write_guard a v f = {
        acquire = (fun m c -> with_mutex a.mutex (fun _ ->
            a.writers <- (m, c) :: a.writers));
        release = (fun m c -> with_mutex a.mutex (fun _ ->
            a.writers <- List.filter ((<>) (m, c)) a.writers));
        attempt = (fun () -> option_map f
            (with_mutex a.mutex (fun _ -> match a.value with
                | Some _ -> None
                | None -> 
                    a.value <- Some v;
                    (match a.readers with
                        | ((m, c)::t) -> with_mutex m (fun _ -> Condition.signal c) 
                        | [] -> ());
                    Some v)));
        }
        
    (* other guard types: time_guard *)

    let read a = prioritized [read_guard a identity] ()
    let write a v = prioritized [write_guard a v ignore] ()

end

exception Foo

let _ = 
    let v = ref 200000 in
    let c = Csp.channel () in
    let c = Csp.channel () in
    Csp.parallel [
        Csp.forever (fun () -> ignore (Csp.read c));
        Csp.forever (fun () -> if !v == 0 then raise Foo else Csp.write c !v; v := !v - 1);
    ] ();
    Csp.prioritized [
        Csp.write_guard c 42 (fun _ -> ())
        Csp.read_guard c (fun v -> print_string (string_of_int v))
    ] ()
    
