module type S = sig
  module Store: Irmin.S

  module Data: sig
    type t = Store.repo
  end

  module Server: Resp_server.SERVER
    with module Auth = Resp_server.Auth.String
    and module Data = Data

  val callback: Data.t -> string -> Hiredis.value array -> Hiredis.value option Lwt.t
end

module Make(I: Irmin.KV): S with module Store = I and type Data.t = I.repo

