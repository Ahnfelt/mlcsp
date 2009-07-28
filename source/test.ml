let _ = 
    let c1 = Csp.new_channel () in
    let c2 = Csp.new_channel () in
    Csp.parallel [
        (fun () -> for i = 1 to 10 do Csp.write c1 "foo" done; print_endline "done");
        Cspu.buffer_process c1 c2 5 [];
        (fun () -> while true do Thread.delay 0.1; print_endline (Csp.read c2) done);
    ]

