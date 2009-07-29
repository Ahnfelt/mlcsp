let rec counter n o () =
  Csp.write o n; counter (n+1) o ()

let rec double i o () =
  let x = Csp.read i in
  Csp.write o (x*2); double i o ()

let rec print i () =
  print_endline(string_of_int (Csp.read i)); print i ()

let even_numbers o () =
  let c = Csp.new_channel () in
    Csp.parallel [
      counter 1 c;
      double c o;
    ]

let _ =
  let c = Csp.new_channel () in
    Csp.parallel [
      even_numbers c;
      print c
    ]
