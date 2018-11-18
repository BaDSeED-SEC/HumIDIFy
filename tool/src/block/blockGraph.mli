open Core_kernel.Std
open Bap.Std

module Node : sig

  type node

  module Arg : sig

    type t = [ `Data of int | `Other ]

    val create  : ?refs:int -> unit -> t
    val is_data : t -> bool
    val n_refs  : t -> int option

  end

  include Opaque
    with type t := node

  val create : ?fn_call:(string * Arg.t list) -> addr -> int -> node

  val id     : node -> int
  val addr   : node -> addr
  val insns  : node -> int
  val args   : node -> Arg.t list option
 
end

module Edge : sig

  type edge
  type t = edge

  include Opaque
    with type t := edge

  val create : unit -> edge

end

module Graph : sig

  include Graph
    with type node = Node.node
     and type Node.label = Node.node
     and type Edge.label = Edge.edge

  val compute_reachable : t -> Node.Set.t Node.Map.t

  (* unique covered nodes are computed using the map of general reachability *)
  val compute_unique    : t -> Node.Set.t Node.Map.t -> Node.Set.t Node.Map.t

  val compute_branch_paths : t -> Node.Set.t Node.Map.t -> Node.t list list Node.Table.t

  val unique            : Node.Set.t Node.Map.t -> Node.t -> Node.t list
  val reachable         : Node.Set.t Node.Map.t -> Node.t -> Node.t list
  val branch_paths      : Node.t list list Node.Table.t -> Node.t -> Node.t list list option

  val decision          : t -> Node.t list -> Node.t list
  val is_decision       : t -> Node.t -> bool

  val data_flow         : ?dir:[ `Forward | `Backward ] ->
                          init:(Node.t -> 'a * 'b) ->
                          trans:(t -> Node.t -> 'a -> 'b -> 'a option) ->
                          join:(t -> Node.t -> 'a -> 'b -> (Node.t * 'a * 'b) seq -> 'b option) ->
                          t ->
                          ('a Node.Map.t * 'b Node.Map.t)

  val of_fn             : IDALink.Function.func -> t

end
