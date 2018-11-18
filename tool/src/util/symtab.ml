open Core_kernel.Std
open Bap.Std

type t = {
  syms_by_name : IDALink.Function.func String.Map.t;
  syms_by_addr : IDALink.Function.func Addr.Map.t;
}

let empty = {
  syms_by_name = String.Map.empty;
  syms_by_addr = Addr.Map.empty;
}

let add t fn = {
  syms_by_name = Map.add t.syms_by_name (IDALink.Function.name fn) fn;
  syms_by_addr = Map.add t.syms_by_addr (IDALink.Function.entry fn) fn;
}
