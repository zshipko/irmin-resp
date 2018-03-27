open Lwt.Infix
open Irmin_server

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Server = Make(Store)

let main =
  let cfg = Irmin_git.config "/tmp/irmin" in
  Store.Repo.v cfg >>= fun repo ->
  Server.Server.create (`TCP (`Port 1234)) repo >>= fun server ->
  Server.Server.run server Server.callback

let _ = Lwt_main.run main

