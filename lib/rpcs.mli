open! Core
open! Async_rpc_kernel

module Client_ready : sig
  module Query : sig
    type t = { name : string ; is_ready : bool } [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t = (string, string) Result.t (* maybe change to custom*) [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end