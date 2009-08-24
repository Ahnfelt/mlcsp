let global_mutex = Mutex.create ()
let find r s =
    Mutex.lock global_mutex;
    try
        let c = Str.regexp r in
        let v = if Str.string_match c s 0
            then
                let rec loop i l = 
                    try loop (i + 1) ((Some (Str.matched_group i s))::l)
                    with 
                        | Not_found -> loop (i + 1) (None::l)
                        | Invalid_argument _ -> List.rev l
                in loop 0 []
            else
                []
        in Mutex.unlock global_mutex; v
    with e -> Mutex.unlock global_mutex; raise e

