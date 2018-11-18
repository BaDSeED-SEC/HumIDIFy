open Core_kernel.Std
open Bap.Std

module rec Function : sig

  type func

  include Opaque
    with type t := func

  val bound  : func -> (addr -> bool)
  val blocks : func -> Block.block Addr.Table.t
  val memory : func -> mem
  val name   : func -> string
  val entry  : func -> addr
  val strings : func -> string list

end and Block : sig

  type block
  type t = block

  include Opaque
    with type t := block

  type dest = [ `Cond of addr * block | `Fall of addr * block | `Call of addr ]

  val addr        : block -> addr
  val local_dests : block -> dest list
  val calls       : image:image -> bound:(addr -> bool) -> block -> addr list
  val dests       : image:image -> bound:(addr -> bool) -> block -> dest list
  val insns       : block -> (mem * insn option) list

  val dependent_call : image:image -> bound:(addr -> bool) -> ftab:[ `Fixed of var list Addr.Table.t | `Heuristic of var list ]  -> block -> (addr * (mem * exp) Var.Map.t) option

end

val load : ?path:string -> image -> string -> Function.func seq option
val load_syms : ?path:string -> string -> string seq option
