module Table = Map.Make(String)

let http_download_process o url () = try
    let spliturl url =
        let re = Str.regexp "\\(http://\\)?\\([^/:]+\\)[:]?\\([^/:]+\\)?\\(/.*\\)?" in
            if Str.string_match re url 0 then
                let host = Str.matched_group 2 url in
                let port = try Str.matched_group 3 url with Not_found -> "80" in 
                let index = try Str.matched_group 4 url with Not_found -> "/" in 
                (host, int_of_string port, index)
            else raise Not_found in
    let sendall socket o =
        let buffer = String.create 512 in
        let rec loop () =
            let count = (Unix.recv socket buffer 0 512 [])
            in if count = 0 then () else begin
                Csp.write o (String.sub buffer 0 count);
                loop ()
            end
        in loop () in
    let (hostname, port, rest) = spliturl url in
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let hostinfo = Unix.gethostbyname hostname in
    let server_address = hostinfo.Unix.h_addr_list.(0) in
    let _ = Unix.connect socket (Unix.ADDR_INET (server_address, port)) in
    let ss = "GET " ^ rest ^ " HTTP/1.0\r\nHost: " ^ hostname ^ "\r\n\r\n" in
        ignore (Unix.send socket ss 0 (String.length ss) []);
        sendall socket o;
        Unix.close socket;
        Csp.poison o
    with _ -> Csp.poison o

let url_process url o () = 
    let c1 = Csp.new_channel () in
    let c2 = Csp.new_channel () in
    let c3 = Csp.new_channel () in try
    let f = Filename.temp_file "csp" "proxy" in
    (try Csp.parallel [
        http_download_process c1 url;
        Cspu.delta c1 c2 c3;
        (fun () -> Csp.write o c2);
        Cspu.write_file f c3
    ] with Csp.PoisonException -> ());
    let rec loop () = 
        let c4 = Csp.new_channel () in
        Csp.parallel [
            Cspu.read_file f c4;
            (fun () -> (try Csp.write o c4 with e -> ()); loop ())
        ]
    in loop ()
    with e -> Csp.poison c1; Csp.poison c2; Csp.poison c3; raise e

let cache_process i o () =
    let rec loop t =
        let u = Csp.read i in
        try let c = Table.find u t in
            Csp.write o (Csp.read c); loop t
        with Not_found -> 
            let c = Csp.new_channel () in
            Csp.parallel [
                url_process u c;
                (fun () -> 
                    Csp.write o (Csp.read c); 
                    loop (Table.add u c t)
                );
            ]
    in loop Table.empty

(* create a server on a given port, and invokes the given function whenever anybody makes a request *)
let socket_listener_process port f () =
    let cache_rpc o i u = Csp.write o u; Csp.read i in
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt socket Unix.SO_REUSEADDR true;
    let hostinfo = Unix.gethostbyname "localhost" in
    let server_address = hostinfo.Unix.h_addr_list.(0) in
        ignore (Unix.bind socket (Unix.ADDR_INET (server_address, port)));
        Unix.listen socket 10;
        let ci = Csp.new_channel () in
        let co = Csp.new_channel () in
        let rec loop () = let (d, _) = Unix.accept socket in
            Csp.parallel [
                f (cache_rpc ci co) (Unix.in_channel_of_descr d) (Unix.out_channel_of_descr d);
                loop;
            ]
        in Csp.parallel [
            cache_process ci co;
            loop;
        ]

let http_request_process rpc i o () =
    let input_header j = let rec loop l = 
            let s = input_line j in
            if s = "" || s = "\r" then l else loop (s::l)
        in List.rev (loop []) in
    let a = List.hd (input_header i) in
    let r = Str.regexp "^GET \\([^ \r\n]+\\)" in
    if Str.string_match r a 0 then begin
        let u = Str.matched_group 1 a in
        let c = rpc u in
        Cspu.finally (fun () -> while true do
            let s = Csp.read c in
            output_string o s;
            flush o
        done) (fun () -> Csp.poison c; close_out o)
    end else raise Not_found

let _ = 
    socket_listener_process 8080 http_request_process ()

