(* BEGIN code modified from http://www.brool.com/index.php/ocaml-sockets *)

let finally f g = (try f () with e -> g (); raise e); g ()
let poison_finally l f = finally f (fun () -> List.iter Csp.poison l)

(* split an url into the (hostname, index) *)
let spliturl url =
    let re = Str.regexp "\\(http://\\)?\\([^/]+\\)\\(/.*\\)?" in
        if Str.string_match re url 0 then
            let host = Str.matched_group 2 url in
            let index = try
                Str.matched_group 3 url
            with Not_found ->
                "/" in
                (host, index)
        else
            raise Not_found

(* read everything pending in the socket *)
let readall socket o =
    let buffer = String.create 512 in
    let rec loop () =
        let count = (Unix.recv socket buffer 0 512 [])
        in if count = 0 then () else begin
            Csp.write o (String.sub buffer 0 count);
            loop ()
        end
    in loop()

(* write everything to a socket *)
let writeall socket s =
    Unix.send socket s 0 (String.length s) []

(* get the contents of an arbitrary URL page *)
let http_download_process url o () = poison_finally [o] (fun () ->
    let (hostname, rest) = spliturl url in
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let hostinfo = Unix.gethostbyname hostname in
    let server_address = hostinfo.Unix.h_addr_list.(0) in
    let _ = Unix.connect socket (Unix.ADDR_INET (server_address, 80)) in
    let ss = "GET " ^ rest ^ " HTTP/1.0\r\nHost: " ^ hostname ^ "\r\n\r\n" in
        ignore (writeall socket ss);
        readall socket o;
        Unix.close socket)

(* END code modified from http://www.brool.com/index.php/ocaml-sockets *)

let _ = 
    let c = Csp.channel () in
    Csp.parallel [
        http_download_process "http://dikurevy.dk/~phillip/revy/smack_my_bits_up_1280x720.m2t" c;
        (fun () -> while true do
            print_string (string_of_int (String.length (Csp.read c)))
        done);
    ]

