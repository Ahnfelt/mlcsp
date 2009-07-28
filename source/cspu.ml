let poison_list l fn () = 
  try fn () with Csp.PoisonException -> List.iter (fun f -> f ()) l

let raise_poison () =
  raise Csp.PoisonException

let pc c () = Csp.poison c

let finally f g = let v = try f () with e -> g (); raise e in g (); v

let conditional_select l =
    let l' = List.filter (fun cg -> cg <> None) l in
    let l'' = List.map (fun cg -> match cg with
        | (Some gp) -> gp 
        | None -> raise (Failure "None")) l' in
    Csp.select l''

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

let write_file f i () =
    let io = try open_out_bin f with e -> Csp.poison i; raise e in
    finally (fun () -> while true do
        output_string io (Csp.read i);
        flush io
    done) (fun () -> Csp.poison i; close_out io)

let read_file f o () =
    let io = try open_in_bin f with e -> Csp.poison o; raise e in
    let size = 512 in
    let b = String.create size in
    finally (fun () ->
        let rec read_chunk s = match s with
        | 0 -> ()
        | _ -> Csp.write o (String.sub b 0 s); read_chunk (input io b 0 size)
    in read_chunk (input io b 0 size)) (fun () -> Csp.poison o; close_in io)

let delta c c1 c2 () = 
    let pl = poison_list [pc c; pc c1; pc c2] in
    try while true do
        let v = Csp.read c in
        Csp.parallel [
            pl (fun () -> Csp.write c1 v);
            pl (fun () -> Csp.write c2 v);
        ]
    done with e -> pl raise_poison (); raise e

let printer c () =
    try while true do print_endline (Csp.read c) done
    with Csp.PoisonException -> Csp.poison c

