let _ = 
    let c1 = Csp.new_channel () in
    Csp.parallel [
        (fun () -> Thread.delay 1.0; Csp.select [Csp.write_guard c1 0 (fun () -> while true do () done)]);
        (fun () -> Csp.read c1; print_endline "done");
    ]

