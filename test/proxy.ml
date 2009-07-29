module Table = Map.Make(String)

(** Reads from i and writes to o. If the value is an empty string, it
    writes (u, n) to d. *)
let registrator i o d u n () =
    try while true do
        match Csp.read i with 
        | Some v -> Csp.write o (Some v)
        | None -> 
            Csp.write o None; 
            Csp.write d (u, n);
            raise Csp.PoisonException
    done with e ->
        Csp.poison i; Csp.poison o; raise e

(** Reads everything from a socket and writes it to another socket while
    putting it in the cache. *)
let socket_downloader fi fo d u () =
    let n = Filename.temp_file "csp" "proxy" in
    let t = open_out n in
    let c1 = Csp.new_channel () in
    let c2 = Csp.new_channel () in
    let c3 = Csp.new_channel () in
    let c4 = Csp.new_channel () in
    Csp.parallel [
        Cspu.file_reader fi c1;
        Cspu.delta c1 c2 c3;
        Cspu.file_writer fo c2;
        registrator c3 c4 d u n;
        Cspu.file_writer t c4
    ]

(** Handles a HTTP/1.0 GET request by responding with the file from the 
    appropriate server. *)
let http_downloader fo u d () =
    let (h, p, i) = match Regex.find 
        "\\(http://\\)?\\([^/:]+\\)[:]?\\([^/:]+\\)?\\(/.*\\)?" u with
        | _ :: _ :: h :: p :: i :: _ -> (
            (match h with Some v -> v | None -> raise Csp.PoisonException),
            (match p with Some v -> v | None -> "80"),
            (match i with Some v -> v | None -> "/"))
        | _ -> raise Csp.PoisonException in
    let header = "GET " ^ i ^ " HTTP/1.0\r\nHost: " ^ h ^ "\r\n\r\n" in
    let port = int_of_string p in
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let hostinfo = Unix.gethostbyname h in
    let server_address = hostinfo.Unix.h_addr_list.(0) in
    let _ = Unix.connect socket (Unix.ADDR_INET (server_address, port)) in
    let c = Csp.new_channel () in
    Csp.parallel [
        Cspu.file_writer (Unix.out_channel_of_descr socket) c;
        (fun () -> Csp.write c (Some header));
        socket_downloader (Unix.in_channel_of_descr socket) fo d u
    ]

(** The handler serves a file, either from the cache or from the 
    appropriate server. *)
let handler fi fo ci co d () =
    let c = Csp.new_channel () in
    Csp.parallel [
        Cspu.file_line_reader fi c;
        (fun () ->
            match Csp.read c with
            | None -> raise Csp.PoisonException
            | Some g ->
            while match Csp.read c with 
                | Some "" | Some "\r" | None -> false 
                | _ -> true 
            do () done;
            Csp.poison c;
            let u = match Regex.find "^GET \\([^ \r\n]+\\)" g with
                | _ :: Some v :: _ -> v
                | _ -> raise Csp.PoisonException in
            Csp.write co u;
            match Csp.read ci with
            | Some n ->
                let c = Csp.new_channel () in
                Csp.parallel [
                    Cspu.file_reader (open_in n) c;
                    Cspu.file_writer fo c
                ]
            | None ->
                http_downloader fo u d ()
        )
    ]

(** This acts as a map from URLs to cache files. When a message is sent
    to it via i, it replies with either Some f where f is the filename
    of the cache file, or with None if it's not in the cache. When it
    receives a message (u, f) over the d channel, it maps the url u to
    the cache file f. *)
let cache i o d () =
    let rec loop t =
        Csp.select [
            Csp.read_guard i (fun u ->
                (try let n = Table.find u t in
                    Csp.write o (Some n)
                with Not_found -> Csp.write o None);
                loop t
            );
            Csp.read_guard d (fun (u, n) ->
                loop (Table.add u n t)
            );
        ]
    in loop Table.empty

(** This process waits for incoming connections on port p and spawns
    handlers that will write back the data from the given URL. It uses
    a subset of the HTTP/1.0 protocol. *)
let gateway p () =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt socket Unix.SO_REUSEADDR true;
    let host_info = Unix.gethostbyname "localhost" in
    let server_address = host_info.Unix.h_addr_list.(0) in
    ignore (Unix.bind socket (Unix.ADDR_INET (server_address, p)));
    Unix.listen socket 10;

    let i = Csp.new_channel () in
    let o = Csp.new_channel () in
    let d = Csp.new_channel () in
    let rec loop () =
        let (f, _) = Unix.accept socket in
        let fi = Unix.in_channel_of_descr f in
        let fo = Unix.out_channel_of_descr f in
        Csp.parallel [
            handler fi fo i o d;
            loop
        ]
    in Csp.parallel [
        cache o i d;
        loop
    ]

(* Program entry point *)
let _ = match Sys.argv with
    | [|_; s|] -> gateway (int_of_string s) ()
    | _ -> print_endline ("USAGE: " ^ Sys.argv.(0) ^ " [port]")

