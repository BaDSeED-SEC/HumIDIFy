open Core_kernel.Std

type state

val load : ida_path:string -> string -> state option
val load_syms : ida_path:string -> string -> String.Set.t option
