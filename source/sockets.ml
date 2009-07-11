(* BEGIN code modified from http://www.brool.com/index.php/ocaml-sockets *)

let finally f g = (try f () with e -> g (); raise e); g ()
let poison_finally l f = finally f (fun () -> List.iter Csp.poison l)

(* split an url into the (hostname, index) *)
let spliturl url =
    let re = Str.regexp "\\(http://\\)?\\([^/:]+\\)[:]?\\([^/:]+\\)?\\(/.*\\)?" in
        if Str.string_match re url 0 then
            let host = Str.matched_group 2 url in
            let port = try Str.matched_group 3 url with Not_found -> "80" in 
            let index = try Str.matched_group 4 url with Not_found -> "/" in 
            (host, int_of_string port, index)
        else raise Not_found

(* read everything pending in the socket *)
let sendall socket o =
    let buffer = String.create 512 in
    let rec loop () =
        let count = (Unix.recv socket buffer 0 512 [])
        in if count = 0 then () else begin
            Csp.write o (String.sub buffer 0 count);
            loop ()
        end
    in loop ()

(* get the contents of an arbitrary URL page *)
let http_download_process o url () = poison_finally [o] (fun () ->
    let (hostname, port, rest) = spliturl url in
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let hostinfo = Unix.gethostbyname hostname in
    let server_address = hostinfo.Unix.h_addr_list.(0) in
    let _ = Unix.connect socket (Unix.ADDR_INET (server_address, port)) in
    let ss = "GET " ^ rest ^ " HTTP/1.0\r\nHost: " ^ hostname ^ "\r\n\r\n" in
        ignore (Unix.send socket ss 0 (String.length ss) []);
        sendall socket o;
        Unix.close socket)

(* create a server on a given port, and invokes the given function whenever anybody makes a request *)
let socket_listener_process port f () =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt socket Unix.SO_REUSEADDR true;
    let hostinfo = Unix.gethostbyname "localhost" in
    let server_address = hostinfo.Unix.h_addr_list.(0) in
        ignore (Unix.bind socket (Unix.ADDR_INET (server_address, port)));
        Unix.listen socket 10;
        let rec loop () = let (d, _) = Unix.accept socket in
            Csp.parallel [
                f (Unix.in_channel_of_descr d, Unix.out_channel_of_descr d);
                loop;
            ]
        in loop ()

(* END code modified from http://www.brool.com/index.php/ocaml-sockets *)

let http_request_process (ic, oc) () =
    let input_header ic = let rec loop l = 
            let s = input_line ic in
            if s = "" || s = "\r" then l else loop (s::l)
        in List.rev (loop []) in
    let a = List.hd (input_header ic) in
    let r = Str.regexp "^GET \\([^ \r\n]+\\)" in
    if Str.string_match r a 0 then begin
        let u = Str.matched_group 1 a in
        let c = Csp.channel () in
        Csp.parallel [
            http_download_process c u;
            (fun () -> while true do
                let s = Csp.read c in
                output_string oc s;
                flush oc
            done)
        ];
        close_out oc
    end else raise Not_found

let _ = 
    socket_listener_process 8080 http_request_process ()

(* API to consider:
    write_process s c
        reads everything from channel c and writes it to socket s,
        closes socket on poison.
    read_process s c
        reads everything from socket s and writes it to channel c,
        closes socket on poison.
    line_process s c
        reads lines from socket s and writes them to channel c
        closes socket on poison.
*)

