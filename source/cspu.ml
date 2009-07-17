(* Quite broken still *)

let poison_list l fn () = 
  try fn () with Csp.PoisonException -> List.iter (fun f -> f ()) l

let poison_channel c () =
  Csp.poison c

let raise_poison () =
  raise Csp.PoisonException

let pc = poison_channel

let finally f g = try f () with e -> g(); raise e

let write_file f i () =
    let io = open_out_bin f in
    try while true do
        output_string io (Csp.read i);
        flush io
    done with e -> close_out io; raise e

let read_file f o () =
    let io = open_in_bin f in
    let size = 512 in
    let b = String.create size in
    let rec read_chunk s = match s with
        | 0 -> Csp.poison o; close_in io
        | _ -> Csp.write o (String.sub b 0 s); read_chunk (input io b 0 size)
    in read_chunk (input io b 0 size)

let read_lines f o () =
    let io = finally (fun () -> open_in_bin f) (fun () -> Csp.poison o) in
    try finally 
        (fun () -> while true do Csp.write o (input_line io) done) 
        (fun () -> Csp.poison o; close_in io) 
    with End_of_file -> ()

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

let _ =
    let c = Csp.channel () in
    Csp.parallel [
        read_lines "test.txt" c;
        printer c;
    ]

