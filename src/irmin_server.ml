(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix
open Hiredis

module type S = sig
  module Store: Irmin.S

  module Backend: sig
    type t = Store.repo
    type client
    val new_client: t -> client
  end

  module Server: Resp_server.SERVER
    with module Auth = Resp_server.Auth.String
    and module Backend = Backend

  val branch: Backend.t -> Backend.client -> Store.t Lwt.t

  val create :
    ?auth: Server.Auth.t ->
    ?host: string ->
    ?tls_config: Conduit_lwt_unix.tls_server_key ->
    Conduit_lwt_unix.server ->
    Backend.t ->
    Server.t Lwt.t

  val create_custom :
    ?auth: Server.Auth.t ->
    ?default: Server.command ->
    ?commands: (string * Server.command) list ->
    ?host: string ->
    ?tls_config: Conduit_lwt_unix.tls_server_key ->
    Conduit_lwt_unix.server ->
    Backend.t ->
    Server.t Lwt.t

  val run :
    ?backlog: int ->
    ?timeout: int ->
    ?stop: unit Lwt.t ->
    ?on_exn: (exn -> unit) ->
    Server.t ->
    unit Lwt.t
end

module Make(Store: Irmin.KV) = struct
  module Store = Store

  module Backend = struct
    type t = Store.repo

    type client = {
      mutable in_multi: bool;
      mutable queue: (string * Hiredis.value array) list;
      mutable txn: Store.t option;
    }

    let new_client _ctx = {
      in_multi = false;
      queue = [];
      txn = None;
    }
  end

  module Server = Resp_server.Make(Resp_server.Auth.String)(Backend)

  (* Internal utility functions *)

  let to_string value =
    let buffer = Buffer.create 1024 in
    let fmt = Format.formatter_of_buffer buffer in
    Store.Contents.pp fmt value;
    Format.pp_print_flush fmt ();
    Buffer.contents buffer

  let branch db client =
    match client.Backend.txn with
    | Some t -> Lwt.return t
    | None -> Store.master db

  let wrap f db client cmd args =
    if client.Backend.in_multi && cmd <> "exec" && cmd <> "discard" then
      let () = client.Backend.queue <- (cmd, args) :: client.Backend.queue in
      Server.ok
    else
      f db client cmd args

  (* Commands *)

  let rec _multi db client cmd args =
    (match args with
    | [| String branch |] ->
        Store.of_branch db branch
    | _ ->
        Store.master db) >>= fun t ->
    client.Backend.txn <- Some t;
    client.Backend.in_multi <- true;
    Server.ok

  and _exec db client cmd args =
    match args with
    | [| |] ->
      client.Backend.in_multi <- false;
      Lwt_list.filter_map_s (fun (cmd, args) ->
        let f = match cmd with
        | "get" -> _get
        | "set" -> _set
        | "remove" -> _remove
        | "list" -> _list
        | _ -> fun db client cmd args -> Lwt.return_none in
        f db client cmd args) (List.rev client.Backend.queue) >>= fun l ->
      client.Backend.queue <- [];
      Lwt.return_some (Value.array (Array.of_list l))
    | _ -> Server.error "Invalid arguments"

  and _discard db client cmd args =
    match args with
    | [| |] ->
      client.Backend.in_multi <- false;
      client.Backend.queue <- [];
      client.Backend.txn <- None;
      Server.ok
    | _ -> Server.error "Invalid arguments"

  and _get db client cmd args =
    match args with
    | [| String key |] ->
      begin
        branch db client >>= fun t ->
        match Store.Key.of_string key with
        | Ok key ->
          (Store.find t key >>= function
            | Some x -> Lwt.return_some (Value.string (to_string x))
            | None -> Lwt.return_some Value.nil)
        | Error (`Msg msg) -> Lwt.return_some (Value.error ("ERR " ^ msg))
      end
    | _ -> Server.error "Invalid arguments"

  and _set db client cmd args =
    match args with
    | [| String key; String value |] ->
      begin
          branch db client >>= fun t ->
          match Store.Key.of_string key with
          | Ok key ->
              (match Store.Contents.of_string value with
              | Ok value ->
                  Store.set t ~info:(Irmin_unix.info ~author:"irmin server" "set") key value >>= fun () ->
                  Lwt.return_some (Value.status "OK")
              | Error (`Msg msg) -> Server.error msg)
          | Error (`Msg msg) -> Server.error msg
      end
    | _ -> Server.error "Invalid arguments"

  and _remove db client cmd args =
    match args with
    | [| String key |] ->
      begin
        branch db client >>= fun t ->
        match Store.Key.of_string key with
        | Ok key ->
            Store.remove t ~info:(Irmin_unix.info ~author:"irmin server" "del") key >>= fun () ->
            Lwt.return_some (Value.status "OK")
        | Error (`Msg msg) -> Server.error msg
      end
    | _ -> Server.error "Invalid arguments"

  and _list db client cmd args =
    match args with
    | [| String kind; String key; |] ->
      begin
        branch db client >>= fun t ->
        match Store.Key.of_string key with
        | Ok key ->
            let buf = Buffer.create 1024 in
            let fmt = Format.formatter_of_buffer buf in
            Store.list t key >>=
            Lwt_list.filter_map_s (fun (s, b) ->
              match b, kind with
              | `Contents, "keys" | `Contents, "all" ->
                Store.Key.pp_step fmt s;
                Format.pp_print_flush fmt ();
                Lwt.return_some (String (Buffer.contents buf))
              | `Node, "dirs" | `Node, "all" -> Lwt.return_none
              | _ -> Lwt.return_none) >>= fun l ->
            Lwt.return_some (Hiredis.Value.array (Array.of_list l))
        | Error (`Msg msg) -> Server.error msg
      end
    | _ -> Server.error "Invalid arguments"

  let commands = [
    "multi", _multi;
    "exec", _exec;
    "discard", _discard;

    "get", wrap _get;
    "set", wrap _set;
    "remove", wrap _remove;
    "list", wrap _list;
  ]

  let create = Server.create ~commands ?default:None
  let create_custom = Server.create
  let run = Server.run
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
