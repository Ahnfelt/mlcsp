(* Enable this to get pretty neat debugging output from mutex locking. *)
(*
module Mutex = struct
    type t = Mutex.t * int * string
    let number = ref 0
    let print s = print_endline (string_of_int (Thread.id (Thread.self ())) ^ " " ^ s)
    let create () = number := !number + 1; (Mutex.create (), !number, "")
    let lock (m, n, s) = print ("locking '" ^ s ^ "' #" ^ string_of_int n); Mutex.lock m; print ("locked '" ^ s ^ "' #"  ^ string_of_int n)
    let unlock (m, n, s) = Mutex.unlock m; print ("unlocked '" ^ s ^ "' #"  ^ string_of_int n)
    let try_lock (m, n, s) = Mutex.try_lock m
end

module Condition = struct
    include Condition
    let wait c (m, n, s) = Mutex.print "waiting"; Condition.wait c m; Mutex.print "not waiting"
end
*)
(*
let with_mutex_print s m f =
    let print s = print_string (string_of_int (Thread.id (Thread.self ()))); print_string " "; print_endline s in
    print s;
    Mutex.lock m;
    finally (fun () -> Mutex.unlock m) (fun () -> f m)
*)

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
    
    val parallel : process list -> process
    (** Runs multiple processes in parallel and returns when they are all done *)
    
    val prioritized : ('a guard) list -> unit -> 'a
    (** Selects one of the guards in the list; if multiple guards are ready, 
        the one with the smallest index in the list is chosen *)

    val read_guard : 'a channel -> 'a guard
    (** Creates a guard that will read from a channel and apply a function. *)
    
    val write_guard : 'a channel -> 'a -> unit guard
    (** Creates a guard that will write to a channel and apply a function. *)
    
    val map_guard : 'a guard -> ('a -> 'b) -> 'b guard
    (** Creates a guard that will write to a channel and apply a function. *)
    
    val read : 'a channel -> 'a
    (** Shortcut for a select with exactly one read guard. *)

    val write : 'a channel -> 'a -> unit
    (** Shortcut for a select with exactly one write guard. *)

end

module Csp : CSP = struct
    
    module type HAPPENING = sig

        type listener
        
        val listener : unit -> listener
        
        val wait : listener -> unit

        type t

        val create : unit -> t
        
        val register : t -> listener -> unit

        val unregister : t -> listener -> unit
        
        val signal : t -> unit

    end

    module Happening : HAPPENING = struct

        type listener = Mutex.t * Condition.t * bool ref
        
        type t = (listener list) ref
        
        let create () = ref [] 
        
        let listener () = (Mutex.create (), Condition.create (), ref false)
        
        let register h x = h := !h @ [x]
        
        let unregister h (_, c, _) = let rec aux l r = match r with
            | [] -> List.rev l
            | (x::xs) -> let (_, c', _) = x in if c' = c then List.rev l @ xs else aux (x::l) xs
            in h := aux [] !h

        let signal h = match !h with
            | ((m, c, v)::_) -> Mutex.lock m; v := true; Condition.signal c; Mutex.unlock m
            | [] -> ()
            
        let wait (m, c, v) = 
            Mutex.lock m;
            while not !v do Condition.wait c m done; 
            v := false;
            Mutex.unlock m
            
    end
    
    (* Do g after f, even in the face of an exception *)
    let finally g f = let v = try f () with e -> g (); raise e in g (); v

    (* Do f while m is locked. Unlocks even in the face of an exception. *)
    let with_mutex m f =
        Mutex.lock m;
        finally (fun () -> Mutex.unlock m) (fun () -> f m)

    type 'a channel = {
        mutex: Mutex.t;
        condition: Condition.t;
        mutable readers: Happening.t;
        mutable writers: Happening.t;
        mutable value: 'a option;
        }

    type 'a guard = {
        acquire: Happening.listener -> unit;
        release: Happening.listener -> unit;
        attempt: unit -> 'a option;
        }

    type process = unit -> unit

    (* TODO: Reuse the current thread. Make sure to handle exceptions right. *)

    let parallel fs () =
        let ts = List.map (fun f -> Thread.create f ()) fs in
        List.iter Thread.join ts

    let channel () = {
        mutex = Mutex.create ();
        condition = Condition.create();
        readers = Happening.create ();
        writers = Happening.create ();
        value = None;
        }

    (* Iterate through a list and return the first whose attempt is successful, 
        or None if no attempts are successful *)
    let rec iterate l = match l with
        | [] -> None
        | (h::t) -> match h.attempt () with
            | Some v -> Some v
            | None -> iterate t

    let prioritized l () =
        let k = Happening.listener () in
        let rec aux () = match iterate l with
            | Some v -> v
            | None -> Happening.wait k; aux () in
        List.iter (fun a -> a.acquire k) l;
        finally (fun () -> List.iter (fun a -> a.release k) l) aux

    (* other selection strategies: round_robin, random, same *)

    (* possible problem below: this locks channel, then select; the other locks select, then channel. *)

    let guard_acquire am ah l = with_mutex am (fun _ -> Happening.register ah l)
    let guard_release am ah l = with_mutex am (fun _ -> Happening.unregister ah l)

    let read_guard a = {
        acquire = guard_acquire a.mutex a.readers;
        release = guard_release a.mutex a.readers;
        attempt = fun l -> with_mutex a.mutex (fun _ -> match a.value with
            | Some v -> 
                a.value <- None;
                Happening.signal a.writers;
                Condition.signal a.condition;
                Some v
            | None -> None)
        }

    let write_guard a v = {
        acquire = guard_acquire a.mutex a.writers;
        release = guard_release a.mutex a.writers;
        attempt = fun l -> with_mutex a.mutex (fun _ -> match a.value with
            | Some _ -> None
            | None -> 
                a.value <- Some v;
                Happening.signal a.readers;
                Condition.wait a.condition a.mutex;
                Some ())
        }
        
    let map_guard g f = {
        acquire = g.acquire;
        release = g.release;
        attempt = fun l -> match g.attempt l with
            | Some v -> Some (f v)
            | None -> None
        }

    (* other guard types: time_guard, possibly a map_guard taking a function and another guard, example:
       map_guard (fun v -> string_of_int v) (read_guard c) *)

    let read a = prioritized [read_guard a] ()
    let write a v = prioritized [write_guard a v] ()

end

exception Foo

let spawn n f () =
    let rec aux n l = if n = 0 then l else aux (n - 1) (f::l) in
    Csp.parallel (aux n []) ()

let rec forever f () = f (); forever f ()

(*
let _ = 
    let c = Csp.channel () in
    Csp.parallel [
        spawn 2 (forever (fun () -> print_string (Csp.read c)));
        spawn 1 (forever (fun () -> Csp.write c "."));
    ] ();*)

let _ = 
    let c1 = Csp.channel () in
    let c2 = Csp.channel () in
    let c3 = Csp.channel () in
    Csp.parallel [
(*        forever (fun () -> Csp.read c1);
        forever (fun () -> Csp.read c1);
        forever (fun () -> Csp.read c1);
        forever (fun () -> Csp.read c1);
        forever (fun () -> Csp.read c1);
        forever (fun () -> Csp.read c1);
        forever (fun () -> Csp.read c1);
        forever (fun () -> Csp.read c1);*)
        forever (fun () -> Csp.write c1 1);
        forever (fun () -> Csp.write c2 1);
        forever (fun () -> Csp.write c2 1);
        forever (Csp.prioritized [
            Csp.map_guard (Csp.read_guard c1) (fun v -> print_endline "<c1>") ;
            Csp.map_guard (Csp.read_guard c2) (fun v -> print_endline "<c2>") ;
        ]);
    ] ();

