(*  
    -> about 15000 - 20000 VM threads, then enters a deadlock-like state.
    -> the memory usage is minimal

    -> about a 100 system threads, then it crashes with an exception.
*)
let loop () = while true do () done

let _ =
    for i = 0 to 1000000000 do 
        ignore (Thread.create loop ());
        if i mod 100 = 0 then print_endline (string_of_int i)
    done

