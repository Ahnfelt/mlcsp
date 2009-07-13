open Csp
open Legoland

let read_file f o () =
  let io = open_in_bin f in
  let b = String.create 512 in
  let rec read_chunk s =
    match s with
      | 0 -> Csp.poison o
      | _ -> Csp.write o (String.sub b 0 s);
          read_chunk (input io b 0 512) in
    read_chunk (input io b 0 512)

let write_file f i () =
  let io = open_out_bin f in
    try
      while true do
        output_string io (Csp.read i);
        flush io
      done
    with PoisonException -> close_out io

let _ =
  let c = Csp.channel () in
    Csp.parallel[
      read_file "a.mp3" c;
      write_file "b.mp3" c
(*
      read_file "test.txt" c;
      write_file "test2.txt" c
*)
    ]
