module Store: Irmin.S

module Data: sig
  type t = Store.repo
end

module Server: Resp_server.SERVER
  with module Data = Data
   and module Auth = Resp_server.Auth.String

val callback: Data.t -> string -> Hiredis.value array -> Hiredis.value option Lwt.t
