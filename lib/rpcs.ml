open! Core
open! Async_rpc_kernel

module Client_ready = struct
  module Query = struct
    type t =
      { name : string
      ; is_ready : bool
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = (string, string) Result.t
    (* maybe change to custom*) [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"ready-status"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Client_connecting = struct
  module Query = struct
    type t = { name : string } [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = (Client_state.t, string) Result.t [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"client-connecting"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

(* handle refreshes, return game state in current phase *)
module Client_message = struct end

module Server_message = struct 
  (* module Query = struct
    type t = {}
  end

  module Response = struct
    type t = {}
  end

  let rpc =  *)
end
