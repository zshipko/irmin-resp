open Lwt.Infix

let main =
    Irmin_server.create (`TCP (`Port 1234)) >>= fun server ->
    Irmin_server.run server (fun srv args ->
        Lwt.return (Some (Hiredis.Value.int 9999)))

let _ = Lwt_main.run main

