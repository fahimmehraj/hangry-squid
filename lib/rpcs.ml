open! Core
open! Async_rpc_kernel

module Client_ready = struct
  module Query = struct
    type t = { name : string ; is_ready : bool } [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = (string, string) Result.t (* maybe change to custom*) [@@deriving sexp_of, bin_io]
  end

  let rpc = 
    Rpc.Rpc.create ~name:"ready-status" ~version:0 ~bin_query:Query.bin_t ~bin_response:Response.bin_t
end

(* handle refreshes, return game state in current phase *)
module Client_message = struct

end

module Client_connecting = struct
  
end

module Server_message = struct 

end
