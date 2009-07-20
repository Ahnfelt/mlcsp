open Event

let rec channelProducer c () =
    Event.sync (Event.send c (Event.new_channel ()));
    print_endline "sent";
    channelProducer c ()

let rec channelConsumer c () =
    Event.sync (Event.receive c);
    print_endline "gotten";
    channelConsumer c ()

let rec producer v c () =
    Event.sync (Event.send c v);
    producer v c ()

let rec printer s c () =
    print_endline s;
    print_endline (Event.sync (Event.receive c));
    printer s c ()

let parallel fs =
    let ts = List.map (fun f -> Thread.create f ()) fs in
    List.iter Thread.join ts

(* Works and alternates randomly between reading from 1 and 2. *)
(*let _ =
    let c = Event.new_channel () in
    parallel [
        printer "a" c;
        printer "b" c;
        producer "1" c;
        producer "2" c;
    ]
*)

let _ =
    let c = Event.new_channel () in
    parallel [
        channelProducer c;
        channelConsumer c;
    ]

