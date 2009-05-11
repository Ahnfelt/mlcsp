open Csp


exception TestException of string

let test_all ts = List.map (fun (n, f) -> try f (); print_endline ("Passed '" ^ n ^ "'") with 
    | TestException t -> print_endline ("Failed '" ^ n ^ "': " ^ t)
    | _ -> print_endline ("Failed '" ^ n ^ "': Exception")) ts

let assert_true c t = if c then () else raise (TestException t)

let assert_contains_all c l = String.contains


let _ = test_all [
    ("any2any", fun () ->
        let c = Csp.channel () in
        let (a, b, c, d, e, f, g, h) = Csp.parallel8
            (fun () -> Csp.read c)
            (fun () -> Csp.write c "r1"; "w1") 
            (fun () -> Csp.write c "r2"; "w2") 
            (fun () -> Csp.read c)
            (fun () -> Csp.read c)
            (fun () -> Csp.write c "r3"; "w3") 
            (fun () -> Csp.read c)
            (fun () -> Csp.write c "r4"; "w4") 
            in
        let s = (a ^ b ^ c ^ d ^ e ^ f ^ g ^ h) in
        let l = ['1'; '2'; '3'; '4'; 'w'; 'r'] in
        assert_true (List.for_all (fun i -> String.contains s i) l) "Broken communication"
    );

    ("capabilities", fun () ->
        let n = ref 0 in
        let printer cin i =
            while true do Csp.read cin; n := !n + 1; done in
        let generator cout () =
            Csp.write cout "1"; 
            Csp.write cout "2"; 
            Csp.write cout "3"; 
            Csp.poison cout; in
        let c = Csp.channel () in
            Csp.fork [
                (fun () -> Csp.spawn 10 (printer (Csp.read_only c)));
                generator (Csp.write_poison_only c);
            ];
        assert_true (!n == 3) "Wrong number of iterations"
    );
    ]

