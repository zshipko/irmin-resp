module Data: sig
  type t = unit
end

val callback: Data.t -> Hiredis.value array -> Hiredis.value option Lwt.t

module Server: Resp_server.SERVER with module Data = Data and module Auth = Resp_server.Auth.String
