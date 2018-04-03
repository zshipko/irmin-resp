open Lwt.Infix
open Irmin_server
open Cmdliner

let select_store: string -> (module Irmin.KV_MAKER) = function
  | "http" -> (module Irmin_unix.Http.KV)
  | "fs" -> (module Irmin_unix.FS.KV)
  | "mem" -> (module Irmin_mem.KV)
  | "git-mem" -> (module Irmin_unix.Git.Mem.KV)
  | "git-fs" | "git" | _ -> (module Irmin_unix.Git.FS.KV)

let select_contents: string -> (module Irmin.Contents.S) = function
  | "cstruct" -> (module Irmin.Contents.Cstruct)
  | _ -> (module Irmin.Contents.String)

let store_type s c: (module S) =
  let (module MakeStore) = select_store s in
  let (module T) = select_contents c in
  (module Make(MakeStore(T)))

let on_exn exc =
  print_string "Error: ";
  print_endline (Printexc.to_string exc)

let main addr port root store contents =
  let (module Server) = store_type store contents in
  let cfg = Irmin_git.config root in
  Server.Store.Repo.v cfg >>= fun repo ->
  Server.create ~host:addr (`TCP (`Port port)) repo >>= fun server ->
  Server.run ~on_exn server

let server addr port root store contents =
  let store = String.lowercase_ascii store in
  let contents = String.lowercase_ascii contents in
  Lwt_main.run (main addr port root store contents)

let addr =
  let doc = "Address to listen on" in
  Arg.(value & opt string "127.0.0.1" & info ["a"; "address"] ~doc ~docv:"HOST")

let port =
  let doc = "Port to listen on" in
  Arg.(value & opt int 6978 & info ["p"; "port"] ~doc ~docv:"PORT")

let root =
  let doc = "Database root path" in
  Arg.(value & opt string "/tmp/irmin" & info ["r"; "root"] ~doc ~docv:"PATH")

let contents =
  let doc = "Content type" in
  Arg.(value & opt string "string" & info ["c"; "contents"] ~doc ~docv:"CONTENTS")

let store =
  let doc = "Backend store type" in
  Arg.(value & opt string "git-fs" & info ["s"; "store"] ~doc ~docv:"STORE")

let server_t = Term.(const server $ addr $ port $ root $ store $ contents)
let cmd = Term.info "irmin-server" ~version:"v0.1" ~exits:Term.default_exits
let () = Term.exit @@ Term.eval (server_t, cmd)
