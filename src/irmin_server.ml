(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix
open Hiredis

module type S = sig
  module Store: Irmin.S

  module Data: sig
    type t = Store.repo
    type client
    val new_client: unit -> client
  end

  module Server: Resp_server.SERVER
    with module Auth = Resp_server.Auth.String
    and module Data = Data

  val callback:
    Data.t ->
    Data.client ->
    string ->
    Hiredis.value array ->
    Hiredis.value option Lwt.t
end

module Make(Store: Irmin.KV) = struct
  module Store = Store

  module Data = struct
    type t = Store.repo

    type client = {
      mutable in_multi: bool;
      mutable queue: (string * Hiredis.value array) list;
      mutable txn: Store.t option;
    }

    let new_client () = {
      in_multi = false;
      queue = [];
      txn = None;
    }
  end

  module Server = Resp_server.Make(Resp_server.Auth.String)(Data)

  let to_string value =
    let buffer = Buffer.create 1024 in
    let fmt = Format.formatter_of_buffer buffer in
    Store.Contents.pp fmt value;
    Format.pp_print_flush fmt ();
    Buffer.contents buffer

  let rec callback db client cmd args =
    let error msg =
      Lwt.return_some (Value.error ("ERR " ^ msg)) in
    let master db client =
      match client.Data.txn with
      | Some t -> Lwt.return t
      | None -> Store.master db in
    if client.Data.in_multi && cmd <> "exec" && cmd <> "discard" then
      let () = client.Data.queue <- (cmd, args) :: client.Data.queue in
      Lwt.return_some (Value.status "OK")
    else
    match cmd, args with
    | "multi", [||] ->
        Store.master db >>= fun t ->
        client.Data.txn <- Some t;
        client.Data.in_multi <- true;
        Lwt.return_some (Value.status "OK")
    | "exec", [||] ->
        client.Data.in_multi <- false;
        Lwt_list.filter_map_s (fun (cmd, args) ->
          callback db client cmd args) (List.rev client.Data.queue) >>= fun l ->
        client.Data.queue <- [];
        Lwt.return_some (Value.array (Array.of_list l))
    | "discard", [||] ->
        client.Data.in_multi <- false;
        client.Data.queue <- [];
        client.Data.txn <- None;
        Lwt.return_some (Value.status "OK")
    | "get", [| String key |] ->
      begin
        master db client >>= fun t ->
        match Store.Key.of_string key with
        | Ok key ->
          (Store.find t key >>= function
            | Some x -> Lwt.return_some (Value.string (to_string x))
            | None -> Lwt.return_some Value.nil)
        | Error (`Msg msg) -> Lwt.return_some (Value.error ("ERR " ^ msg))
      end
    | "set", [| String key; String value |] ->
        begin
          master db client >>= fun t ->
          match Store.Key.of_string key with
          | Ok key ->
              (match Store.Contents.of_string value with
              | Ok value ->
                  Store.set t ~info:(Irmin_unix.info ~author:"irmin server" "set") key value >>= fun () ->
                  Lwt.return_some (Value.status "OK")
              | Error (`Msg msg) -> error msg)
          | Error (`Msg msg) -> error msg
        end
    | "remove", [| String key |] ->
        begin
          master db client >>= fun t ->
          match Store.Key.of_string key with
          | Ok key ->
              Store.remove t ~info:(Irmin_unix.info ~author:"irmin server" "del") key >>= fun () ->
              Lwt.return_some (Value.status "OK")
          | Error (`Msg msg) -> Lwt.return_some (Value.error ("ERR " ^ msg))
        end
    | _, _ -> Lwt.return_some (Value.error "ERR invalid command")
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
