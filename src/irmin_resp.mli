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

module Make(I: Irmin.S): S with module Store = I and type Backend.t = I.repo

