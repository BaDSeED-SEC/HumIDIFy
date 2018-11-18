type environment

val default_environment : environment

val load_profile        : environment -> string -> environment
val evaluate_profile    : ida_path:string -> environment -> string -> (Elf.SectionHeader.t array * (Bap.Std.Addr.t -> bool) * Elf.StringTable.t * Bap.Std.image * Symtab.t * Core_kernel.Std.String.Set.t * Core_kernel.Std.String.Set.t) -> string -> bool option
