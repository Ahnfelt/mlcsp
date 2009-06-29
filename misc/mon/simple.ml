(* one2one channel with alternation *)

(* in order to try this out we need a process with two input
   channels, two input guards. In case both channels are ready
   to write, we will read from the first, even if it's unfair *)

exception Simple

type 'a channel = {
  mutable r: 'a option; (* None / Some *)
  mutable w: 'a option; (* None / Some *)
  mutable v: 'a option  (* None / Some *)
}

type 'a alternate = {
  mutable l: ('a channel) list
}

let channel () = {
  r = None;
  w = None;
  v = None
}

let alternate () = {
  l = [];
}


let parallel l () =
  let ts = List.map (fun f -> Thread.create f ()) l in
    List.iter Thread.join ts

let _ =
  let c1 = channel () in
  let c2 = channel () in
  let g = alternate () in
  g.l <- g.l @ [c1] @ [c2];
  parallel [
    
  ] ()
