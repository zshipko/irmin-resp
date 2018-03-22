type t

val create :
    ?auth:string ->
    ?host:string ->
    ?tls_config:Conduit_lwt_unix.tls_server_key ->
    Conduit_lwt_unix.server ->
    t Lwt.t

val run :
    ?backlog:int ->
    ?timeout:int ->
    ?stop:unit Lwt.t ->
    ?on_exn:(exn -> unit) ->
    t ->
    (t -> Hiredis.value array -> Hiredis.value option Lwt.t) ->
    unit Lwt.t
