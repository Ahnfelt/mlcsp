(* Build: ocamlc -vmthread threads.cma -c csp.mli csp.ml cspu.ml 
   Append myfile.ml -o myfile to use it in your own programs. *)

(** Reads from the input channel and repeats the message in parallel to
    both o1 and o2. When both have read, it does the whole thing again. *)
let delta i o1 o2 () =
    try while true do
        let v = Csp.read i in
        Csp.parallel [
            (fun () -> Csp.write o1 v);
            (fun () -> Csp.write o2 v);
        ]
    done with Csp.PoisonException -> 
        Csp.poison i; Csp.poison o1; Csp.poison o2

(** Reads a whole file, writing it in non-zero length chunks over the 
    channel wrapped in Some. When the end of the file is reached, it 
    closes the file, sends one None message, poisons the channel and 
    stops. In case of error, it poisons the channel and stops. *)
let file_reader f o () =
    let size = 1024 in
    let b = String.create size in
    try while true do
        let s = input f b 0 size in
        if s = 0 then raise End_of_file
        else Csp.write o (Some (String.sub b 0 s))
    done with 
        | End_of_file -> Csp.write o None; Csp.poison o; close_in f
        | e -> Csp.poison o; close_in f; raise e

(** Like read_file, but reads lines instead of randomly sized chunks of
    data. The end-of-line character is not included in the string. *)
let file_line_reader f o () = 
    try while true do
        let b = input_line f in
        Csp.write o (Some b)
    done with
        | End_of_file -> Csp.write o None; Csp.poison o; close_in f
        | e -> Csp.poison o; close_in f; raise e

(** Writes each Some chunk it receives over the channel to the file.
    When it receives None or the channel is poisoned, it closes the file 
    and poisons the channel. *)
let file_writer f i () =
    try while true do
        match Csp.read i with
        | Some b -> output_string f b; flush f
        | None -> raise Csp.PoisonException
    done with
        | e -> Csp.poison i; close_out f; raise e

(** Prints out strings read from the channels in a loop. *)
let printer c () =
    try while true do print_endline (Csp.read c) done
    with Csp.PoisonException -> Csp.poison c

(** Like select, but only between those alternatives that are not
    specified as None. *)
let conditional_select l =
    let l' = List.filter (fun cg -> cg <> None) l in
    let l'' = List.map (fun cg -> match cg with
        | (Some gp) -> gp 
        | None -> raise (Failure "None")) l' in
    Csp.select l''

(** A process that acts as a buffer of size n between channels i and
    o, with the initial state l. *)
let rec buffer_process i o n l () =
    conditional_select [
        (if l <> [] then Some (
            Csp.write_guard o (List.hd l) (fun () -> 
                buffer_process i o n (List.tl l) ()
            )
        ) else None);
        (if List.length l < n then Some (
            Csp.read_guard i (fun h -> 
                buffer_process i o n (l @ [h]) ()
            )
        ) else None);
    ]

