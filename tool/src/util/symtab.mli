open Core_kernel.Std
open Bap.Std

type t = {
  syms_by_name : IDALink.Function.func String.Map.t;
  syms_by_addr : IDALink.Function.func Addr.Map.t;
}

val empty : t
val add   : t -> IDALink.Function.func -> t
