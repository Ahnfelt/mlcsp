(* Fibonacci CSP network *)

open Fib

let rec fibcsp n =
  match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fibcsp(n - 1) + fibcsp(n - 2)

let _ = print_endline (string_of_int(fibcsp (10)))
