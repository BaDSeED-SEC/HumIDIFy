open Core_kernel.Std
open Bap.Std

module Arm : sig

  val default_retn      : var

  val resolve_pc        : mem -> stmt list -> stmt list
  val fold_constants    : stmt list -> stmt list
  val resolve_indirects : 'a memmap -> stmt list -> stmt list

  val resolve_calls     : memory:'a memmap -> bound:(addr -> bool) -> (mem * insn) list -> addr list

  val find_dependent_call : memory: 'a memmap -> bound:(addr -> bool) -> ftab:[ `Fixed of var list Addr.Table.t | `Heuristic of var list ] -> (mem * insn) list -> (addr * (mem * exp) Var.Map.t) option

  val zcmp_dest         : memory:'a memmap -> dest1:addr -> dest2:addr -> (mem * insn) list -> addr option

end
