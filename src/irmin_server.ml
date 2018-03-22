(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix
open Hiredis

open Unix

type t = {
    s_ctx: Conduit_lwt_unix.ctx;
    s_mode: Conduit_lwt_unix.server;
    s_tls_config: Conduit_lwt_unix.tls_server_key option;
    s_auth: string option;
}

let create ?auth ?host:(host="127.0.0.1") ?tls_config mode =
    Conduit_lwt_unix.init ~src:host ?tls_server_key:tls_config () >|= fun ctx ->
    {
        s_ctx = ctx;
        s_mode = mode;
        s_tls_config = tls_config;
        s_auth = auth;
    }

let buffer_size = 1024

let rec read ic =
    Lwt_io.read ~count:buffer_size ic >>= fun s ->
    if String.length s = buffer_size then
        read ic >|= fun s' -> s ^ s'
    else Lwt.return s

let rec aux srv authenticated callback ic oc r =
    Lwt_io.read ~count:buffer_size ic >>= fun s ->
    if String.length s <= 0 then
        Lwt.return_unit
    else
    let () = ignore (Reader.feed r s) in
    match Reader.get_reply r with
    | None ->
        aux srv authenticated callback ic oc r
    | Some (Array a) ->
        if authenticated then
            (callback srv a >>= function
            | Some res ->
                Lwt_io.write oc (encode_string res) >>= fun _ ->
                aux srv true callback ic oc r
            | None ->
                print_endline "B";
                Lwt.return_unit)
        else begin match a with
            | [| (String "AUTH"|String "auth"); String x |] when Some x = srv.s_auth ->
                Lwt_io.write oc (encode_string (Status "OK")) >>= fun _ ->
                aux srv true callback ic oc r
            | _ ->
                Lwt_io.write oc (encode_string (Error "NOAUTH Authentication Required"))
                >>= fun _ -> aux srv false callback ic oc r
        end
    | _ ->
        Lwt_io.write oc (encode_string (Error "NOCOMMAND Invalid Command")) >>= fun _ ->
        Lwt.return_unit

let rec handle srv callback flow ic oc =
    let r = Reader.create () in
    Lwt.catch (fun () ->
        aux srv (srv.s_auth = None) callback ic oc r)
    (fun _ ->
        Lwt_unix.yield ())

let run ?backlog ?timeout ?stop ?on_exn srv callback =
    Conduit_lwt_unix.serve ?backlog ?timeout ?stop ?on_exn
        ~ctx:srv.s_ctx ~mode:srv.s_mode (handle srv callback)

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
