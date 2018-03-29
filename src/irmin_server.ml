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

  val callback: Data.t -> Data.client -> string -> Hiredis.value array -> Hiredis.value option Lwt.t
end

module Make(Store: Irmin.KV) = struct
  module Store = Store

  module Data = struct
    type t = Store.repo

    type client = {
      queue: Hiredis.value array Queue.t;
    }

    let new_client () = {
      queue = Queue.create ()
    }
  end

  module Server = Resp_server.Make(Resp_server.Auth.String)(Data)

  let to_string value =
    let buffer = Buffer.create 1024 in
    let fmt = Format.formatter_of_buffer buffer in
    Store.Contents.pp fmt value;
    Buffer.contents buffer

  let callback db client cmd args =
    let error msg =
      Lwt.return_some (Value.error ("ERR " ^ msg)) in
    match cmd, args with
    | "get", [| String key |] ->
      begin
        Store.master db >>= fun t ->
        match Store.Key.of_string key with
        | Ok key ->
          (Store.find t key >>= function
            | Some x -> Lwt.return_some (Value.string (to_string x))
            | None -> Lwt.return_some Value.nil)
        | Error (`Msg msg) -> Lwt.return_some (Value.error ("ERR " ^ msg))
      end
    | "set", [| String key; String value |] ->
        begin
          Store.master db >>= fun t ->
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
          Store.master db >>= fun t ->
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
