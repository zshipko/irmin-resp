module Data: sig
  type t = unit
end

val callback: Data.t -> Hiredis.value array -> Hiredis.value option Lwt.t

module Server: Resp_server.SERVER with type data = Data.t
