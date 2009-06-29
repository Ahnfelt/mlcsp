(* Proxy network *)

open List
open Printf
open Csp

let fi x = float_of_int x

let read_file f n g () =
  let io = open_in f in
    try
      while true do
        print_endline(input_line io);
        Thread.delay (fi 1 /. fi n);
        Thread.delay (fi 1 /. fi g)
      done
    with End_of_file -> close_in io

let _ =
  let g = 1000 in 
  let l = [("test1.txt",25);("test2.txt",2000);("test3.txt",500)] in
  List.iter (fun (f,n) -> read_file f n g ()) l
