open Unix

(* Waits for user input *)
let a () =
    let i = in_channel_of_descr stdin in
    print_endline "read from stdin";
    print_endline (input_line i)

(* Reads while waiting for user input *)
let b () = 
    let i = open_in "nonblocking.ml" in
    Thread.delay 1.0;
    print_endline "read from file";
    print_endline (input_line i)

(* Run the two threads in parallel *)
let _ = 
    let t1 = Thread.create a () in
    let t2 = Thread.create b () in
    Thread.join t1; Thread.join t2

