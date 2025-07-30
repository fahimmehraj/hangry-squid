open! Core
open! Async_rpc_kernel

module Client_message = struct
  module Ready_status_change = struct
    type t =
      { name : string
      ; is_ready : bool
      }
    [@@deriving sexp_of, bin_io]
  end

  module Item_selection = struct
    type t =
      { name : string
      ; item : Item.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Query = struct
    type t =
      | New_player of string
      | Ready_status_change of Ready_status_change.t
      | Item_selection of Item_selection.t
      | Chat_message of Message.t
      | Item_used of Action.t
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = (string, string) Result.t
    (* maybe change to custom*)
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"ready-status"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
      ~include_in_error_count:Only_on_exn
  ;;
end

module Poll_client_state = struct
  module Query = struct
    type t = { name : string } [@@deriving sexp, bin_io, equal]
  end

  module Response = struct
    type t = Client_state.t [@@deriving sexp, bin_io, equal]

    module Update = struct
      type nonrec t = t [@@deriving sexp, bin_io, equal]
    end

    let diffs ~from ~to_ = to_
    let update t update = update
  end

  module Error = struct
    type t = string [@@deriving sexp, bin_io]
  end

  let rpc =
    Polling_state_rpc.create
      ~name:"ready-status"
      ~version:0
      ~query_equal:[%equal: Query.t]
      ~bin_query:Query.bin_t
      (module Response)
  ;;
end
