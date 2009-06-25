let id x = x
let s x _ = x
let _ =
    let c1 = Csp.channel () in
    let c2 = Csp.channel () in
    let c3 = Csp.channel () in
    Csp.parallel [
        
        (let rec loop () = let v = Csp.select [
            Csp.read_guard c1 id;
            Csp.read_guard c2 id;
            Csp.write_guard c3 "a 3" (s "a write 3");
        ] in print_endline v; loop () in loop);
        
        (let rec loop () = let v = Csp.select [
            Csp.read_guard c3 id;
            Csp.read_guard c2 id;
            Csp.write_guard c1 "b 1" (s "b write 1");
        ] in print_endline v; loop () in loop);
        
        (let rec loop () = let v = Csp.select [
            Csp.write_guard c1 "c 1" (s "c write 1");
            Csp.write_guard c2 "c 2" (s "c write 2");
        ] in print_endline v; loop () in loop);
        
        (*fun () -> Thread.delay 5.0; Csp.poison c2*)
    ]

