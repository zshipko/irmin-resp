(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix
open Resp_server
open Value

module type S = sig
  module Store: Irmin.S
  module Sync: Irmin.SYNC with type db = Store.t

  module Backend: sig
    type t = Store.repo
    type client
    val new_client: t -> client
  end

  module Server: Resp_server.SERVER
    with module Auth = Resp_server.Auth.String
    and module Backend = Backend

  val commands: unit -> (string * Server.command) list
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

  val start:
    ?backlog: int ->
    ?timeout: int ->
    ?stop: unit Lwt.t ->
    ?on_exn: (exn -> unit) ->
    Server.t ->
    unit Lwt.t
end

module Make(Store: Irmin.S) = struct
  module Store = Store
  module Sync = Irmin.Sync(Store)

  module Backend = struct
    type t = Store.repo

    type client = {
      mutable in_multi: bool;
      mutable queue: (string * Value.t array) list;
      mutable branch: Store.t option;
      mutable commit_info: (string * string) option;
      mutable tree: Store.tree option;
    }

    let new_client _ctx = {
      in_multi = false;
      queue = [];
      branch = None;
      commit_info = None;
      tree = None;
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
    match client.Backend.branch with
    | Some t -> Lwt.return t
    | None ->
        Store.master db >|= fun m ->
        client.Backend.branch <- Some m;
        m

  let tree db client =
    match client.Backend.tree with
    | Some tree -> Lwt.return tree
    | None -> Store.tree db

  let commit_info client ?author:(author="irmin-resp") message =
    let author, message = match client.Backend.commit_info with
      | Some (a, m) -> a, m
      | None -> author, message
    in
    Irmin_unix.info ~author "%s" message

  let wrap f db client cmd args =
    if client.Backend.in_multi && cmd <> "multi" && cmd <> "exec" && cmd <> "discard" then
      let () = client.Backend.queue <- (cmd, args) :: client.Backend.queue in
      Server.ok
    else
      f db client cmd args >>= fun res ->
      (match client.Backend.tree with
      | Some tree ->
          let info = commit_info client "exec"  in
          branch db client >>= fun t ->
          Store.set_tree t Store.Key.empty tree ~info
      | None -> Lwt.return_unit) >|= fun () -> res

  (* Commands *)

  let rec _multi db client cmd args =
    let get_branch branch =
        if branch = "master" then
          Store.master db
        else
          (match Store.Branch.of_string branch with
          | Ok branch -> Store.of_branch db branch
          | Error (`Msg msg) -> failwith msg)
    in
    Lwt.catch (fun () ->
      if not client.Backend.in_multi then
        (match args with
        | [| String branch |] ->
            get_branch branch
        | [| String branch; String author; String message |] ->
            client.Backend.commit_info <- Some (author, message);
            get_branch branch
        | _ ->
            Store.master db) >>= fun t ->
        client.Backend.branch <- Some t;
        client.Backend.in_multi <- true;
        Server.ok
      else
        Server.ok)
    (function
      | Failure msg -> Server.error msg
      | exn -> raise exn)

  and _exec db client cmd args =
    match args with
    | [| |] ->
      client.Backend.in_multi <- false;
      let cmds = commands () in
      branch db client >>= fun t ->
      Store.tree t >>= fun tree ->
      client.Backend.tree <- Some tree;
      Lwt_list.filter_map_s (fun (cmd, args) ->
        if cmd = "multi" || cmd = "exec" || cmd = "discard" then
          Lwt.return_none
        else
          match List.assoc_opt cmd cmds with
          | Some f -> f db client cmd args
          | None -> Lwt.return_none)
      (List.rev client.Backend.queue) >>= fun l ->
      (match client.Backend.tree with
      | Some tree ->
          let info = commit_info client "exec"  in
          Store.set_tree t Store.Key.empty tree ~info
      | None -> Lwt.return_unit) >>= fun () ->
      client.Backend.queue <- [];
      client.Backend.commit_info <- None;
      client.Backend.branch <- None;
      client.Backend.tree <- None;
      Lwt.return_some (Value.array (Array.of_list l))
    | _ -> Server.error "Invalid arguments"

  and _discard db client cmd args =
    match args with
    | [| |] ->
      client.Backend.in_multi <- false;
      client.Backend.queue <- [];
      client.Backend.branch <- None;
      client.Backend.commit_info <- None;
      client.Backend.tree <- None;
      Server.ok
    | _ -> Server.error "Invalid arguments"

  and _pull db client cmd args =
    let rec aux args =
      let uri, mode = match args with
      | [| String uri; String merge |] ->
          let info = `Merge (commit_info client ("pull: " ^  uri)) in
          uri, info
      | [| String uri |] ->
          uri, `Set
      | _ -> (* Invalid arguments *)
          "", `Set
      in
      if uri = "" then Server.error "Invalid arguments"
      else
        branch db client >>= fun t ->
        Sync.pull t (Irmin.remote_uri uri) mode >>= function
        | Result.Ok _ -> Server.ok
        | Result.Error (`Msg msg) -> Server.error ("Unable to pull: " ^ msg)
        | Result.Error _ -> Server.error "Unable to pull"
    in
    aux args

  and _head db client cmd args =
    branch db client >>= fun t ->
    Store.Head.find t >>= function
    | Some commit ->
        let s = Store.Commit.hash commit |> Fmt.strf "%a" Store.Commit.Hash.pp in
        Lwt.return_some (String s)
    | None -> Lwt.return_some Nil

  and _branch db client cmd args =
    match args with
    | [| String "LIST" |] ->
      begin
        branch db client >>= fun t ->
        let repo = Store.repo t in
        Store.Branch.list repo >>=
        Lwt_list.map_s (fun x ->
          Lwt.return (String (Fmt.strf "%a" Store.Branch.pp x))) >>= fun branches ->
        Lwt.return_some (Array (Array.of_list branches))
      end
    | _ -> Server.error "Invalid arguments"

  and _get db client cmd args =
    match args with
    | [| String key |] ->
      begin
        branch db client >>= fun t ->
        tree t client >>= fun tree ->
        match Store.Key.of_string key with
        | Ok key ->
          (Store.Tree.find tree key >>= function
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
          tree t client >>= fun tree ->
          match Store.Key.of_string key with
          | Ok key ->
              (match Store.Contents.of_string value with
              | Ok value ->
                  Store.Tree.add tree key value >>= fun tree ->
                  client.Backend.tree <- Some tree;
                  Server.ok
              | Error (`Msg msg) -> Server.error msg)
          | Error (`Msg msg) -> Server.error msg
      end
    | _ -> Server.error "Invalid arguments"

  and _remove db client cmd args =
    match args with
    | [| String key |] ->
      begin
        branch db client >>= fun t ->
        tree t client >>= fun tree ->
        match Store.Key.of_string key with
        | Ok key ->
            Store.Tree.remove tree key >>= fun tree ->
            client.Backend.tree <- Some tree;
            Server.ok
        | Error (`Msg msg) -> Server.error msg
      end
    | _ -> Server.error "Invalid arguments"

  and _list db client cmd args =
    match args with
    | [| String key |] ->
        _list db client cmd (Array.append [| String "all" |] args)
    | [| String kind; String key; |] ->
      begin
        branch db client >>= fun t ->
        tree t client >>= fun tree ->
        match Store.Key.of_string key with
        | Ok key ->
            let buf = Buffer.create 1024 in
            let fmt = Format.formatter_of_buffer buf in
            let format_key s =
                Store.Key.pp_step fmt s;
                Format.pp_print_flush fmt ();
                let s' = Buffer.contents buf in
                let () = Buffer.clear buf in
                Lwt.return_some (String s')
            in
            Store.Tree.list tree key >>=
            Lwt_list.filter_map_s (fun (s, b) ->
              match b, kind with
              | `Contents, "keys" | `Contents, "all" -> format_key s
              | `Node, "dirs" | `Node, "all" -> format_key s
              | _ -> Lwt.return_none) >>= fun l ->
            Lwt.return_some (Value.array (Array.of_list l))
        | Error (`Msg msg) -> Server.error msg
      end
    | _ -> Server.error "Invalid arguments"

  and commands () = [
    "multi", _multi;
    "exec", _exec;
    "discard", _discard;

    "pull", wrap _pull;
    "head", wrap _head;
    "branch", wrap _branch;

    "get", wrap _get;
    "set", wrap _set;
    "remove", wrap _remove;
    "list", wrap _list;
    (*"metadata", wrap _metadata;*)
    (*"kind", wrap _kind;*)
  ]

  let create = Server.create ~commands:(commands ()) ?default:None
  let create_custom = Server.create
  let start = Server.start
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
