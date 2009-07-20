open Cspu
open Legoland

let _ = print_endline "Commstime"

let consumer i () =
  let pl = poison_list [pc i] in
  let n = 5000 in
  let ts = Unix.time in
  let _ = Csp.read i in
  let t1 = ts () in
    for j = 0 to n do
      Csp.read i
    done;
    let t2 = ts () in
    let dt = t2 -. t1 in
    let tchan = dt /. (4.0 *. float_of_int n) in
      Printf.printf "DT = %f.\nTime per ch : %f/(4*%d) = %f s = %f us\n"
        dt dt n tchan (tchan *. 1000000.0);
      Printf.printf "consumer done, poisoning channel";
      pl raise_poison()
      
let _ = 
  let a = Csp.channel () in
  let b = Csp.channel () in
  let c = Csp.channel () in
  let d = Csp.channel () in
      Csp.parallel [
        prefixint (bii 0) c a;
        delta2int a b d;
        succint b c;
        consumer d
      ]
