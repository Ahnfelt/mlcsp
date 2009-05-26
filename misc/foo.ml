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

type 'a channel = {
    lock: Mutex.t;
    mutable read_conditions: Condition.t list;
    mutable write_conditions: Condition.t list;
    mutable value: 'a option;
    }

type 'a guard = {
    acquire: Condition.t -> unit;
    release: Condition.t -> unit;
    attempt: Mutex.t -> 'a option;
    }

let channel () = {
    lock = Mutex.create ();
    read_conditions = [];
    write_conditions = [];
    value = None;
    }

let with_mutex m f = (
    Mutex.lock m;
    let v = try f m with e -> Mutex.unlock m; raise e in 
    Mutex.unlock m;
    v)

let rec iterate l m = match l with
    | (h::t) -> (match h.attempt m with
        | Some v -> Some v
        | None -> iterate t m)
    | [] -> None

let prioritized l =
    let rec aux c m = match iterate l m with
        | Some v -> v
        | None -> Condition.wait c m; aux c m in 
    let m = Mutex.create () in
    let c = Condition.create () in
    List.iter (fun a -> a.acquire c) l;
    let v = with_mutex m (aux c) in
    List.iter (fun a -> a.release c) l;
    v

let read_guard a f = {
    acquire = (fun c -> with_mutex a.lock (fun _ ->
        a.read_conditions <- c :: a.read_conditions));
    release = (fun c -> with_mutex a.lock (fun _ ->
        a.read_conditions <- List.filter ((<>) c) a.read_conditions));
    attempt = (fun m -> with_mutex a.lock (fun _ ->
        with_mutex m (fun _ -> match a.value with
            | Some v -> 
                a.value <- None; 
                Condition.signal (List.hd a.write_conditions); 
                Some (f v)
            | None -> None)));
    }

let write_guard a v f = {
    acquire = (fun c -> with_mutex a.lock (fun _ ->
        a.write_conditions <- c :: a.write_conditions));
    release = (fun c -> with_mutex a.lock (fun _ ->
        a.write_conditions <- List.filter ((<>) c) a.write_conditions));
    attempt = (fun m -> with_mutex a.lock (fun _ ->
        with_mutex m (fun _ -> match a.value with
            | Some _ -> None
            | None -> 
                a.value <- Some v;
                Condition.signal (List.hd a.read_conditions);
                Some (f v))));
    }

let read a = prioritized [read_guard a (fun v -> v)]
let write a v = prioritized [write_guard a v (fun _ -> ())]

let _ = 
    let c = channel () in
    parallel
        (let rec f () = print_string (read c); f () in f)
        (let rec f () = write c "foo"; f () in f)

