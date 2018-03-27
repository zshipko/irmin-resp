open Lwt.Infix
open Irmin_server

let main =
  let cfg = Irmin_git.config "/tmp/irmin" in
  Store.Repo.v cfg >>= fun repo ->
  Server.create (`TCP (`Port 1234)) repo >>= fun server ->
  Server.run server callback

let _ = Lwt_main.run main

