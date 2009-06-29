(* Proxy network *)

open Big_int
open List
open Printf
open Csp

let fi x = float_of_int x
let bii x = big_int_of_int x
let sbi x = string_of_big_int x
let (++) x y = add_big_int x y

let read_file f n o () =
  let io = open_in f in
    try
      while true do
        Csp.write o (input_line io, n)
      done
    with End_of_file -> close_in io

(* throughput or network throughput is the average rate of successful message
   delivery over a communication channel. *)
let rec throughput n i o () =
  Csp.write o (Csp.read i);
  (* We set v = 1/n (n is in Kilobits ) and we put the process to sleep v. It's
     a naive way to calculate represent broadband bandwidth. *)
  Thread.delay (fi 1 /. fi n);
  throughput n i o ()

let buffer i o () =
  (* we always choose the first file as primary select (alternative) any2one *)
  let g = List.map (fun c -> read_guard c (fun x -> x)) i in
    while true do
      Csp.write o (Csp.select g)
    done

let rec sink i () =
  let (l,n) = Csp.read i in
    print_endline(l);
  sink i ()

(* Main process which contain all the others and hereby forms a proxy  *)
let proxy l n () =
  let tp = Csp.channel () in
  let bt = Csp.channel () in
  (* TODO: dynamic create list of channels *)
  let pc = List.map (fun (f,n) ->
                      let c1 = Csp.channel () in
                      let c2 = Csp.channel () in
                      let f = (fun () -> Csp.parallel[
                                 read_file f n c1;
                                 throughput n c1 c2;
                               ]) in
                        (f, c2)) l in
  let (p,c) = List.split pc in
    Csp.parallel (buffer c bt::throughput n bt tp::sink tp::p)
  (* We give a list of pairs with the name of the file and the
     throughput for the file. We then perform priSelect always on the
     first file of the list. We then try to push through the proxies
     throughput and in first place it will go away (/dev/null) but at
     some point we will do something with the data.

     The point is, if the first file is slow to read we will actually
     read from one of the others *) 

let _ =
  let l = [("test1.txt",25);("test2.txt",2000);("test3.txt",500)] in
  let n = 1000 in
  proxy l n ();
