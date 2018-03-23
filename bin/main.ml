open Lwt.Infix
open Irmin_server

let main =
  Server.create (`TCP (`Port 1234)) () >>= fun server ->
  Server.run server callback

let _ = Lwt_main.run main

