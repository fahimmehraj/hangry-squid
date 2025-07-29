open! Core
open! Async_rpc_kernel

module Client_message : sig
  module Ready_status_change : sig
    type t =
      { name : string
      ; is_ready : bool
      }
    [@@deriving sexp_of, bin_io]
  end

  module Item_selection : sig
    type t =
      { name : string
      ; item : Item.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Query : sig
    type t =
      | Ready_status_change of Ready_status_change.t
      | Item_selection of Item_selection.t
      | Chat_message of Message.t
      | Item_used of Action.t
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t = (string, string) Result.t
    (* maybe change to custom*) [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module State_pipe : sig
  module Query : sig
    type t = { name : string } [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t = Client_state.t [@@deriving sexp_of, bin_io]
  end

  module Error : sig
    type t = string [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t, Error.t) Rpc.Pipe_rpc.t
end
