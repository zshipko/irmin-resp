open Lwt.Infix
open Irmin_server
open Cmdliner

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Server = Make(Store)

let main addr port root =
  let (module Server): (module S) = if false then (module Make(Irmin_unix.Git.Mem.KV(Irmin.Contents.String))) else (module (Make(Irmin_unix.Git.FS.KV(Irmin.Contents.String)))) in
  let cfg = Irmin_git.config root in
  Server.Store.Repo.v cfg >>= fun repo ->
  Server.create ~host:addr (`TCP (`Port port)) repo >>= fun server ->
  Server.run server

let server addr port root = Lwt_main.run (main addr port root)

let addr =
  let doc = "Address to listen on" in
  Arg.(value & opt string "127.0.0.1" & info ["a"; "address"] ~doc ~docv:"ADDR")

let port =
  let doc = "Port to listen on" in
  Arg.(value & opt int 6978 & info ["p"; "port"] ~doc ~docv:"PORT")

let root =
  let doc = "Database root path" in
  Arg.(value & opt string "/tmp/irmin" & info ["r"; "root"] ~doc ~docv:"ROOT")

let server_t = Term.(const server $ addr $ port $ root)
let cmd =   Term.info "irmin-server" ~version:"v0.1" ~exits:Term.default_exits
let () = Term.exit @@ Term.eval (server_t, cmd)
