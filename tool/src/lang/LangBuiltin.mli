type ctx

val function_args  : ctx -> string -> (string * LangAst.ty) option array -> (string * LangBuiltinBase.t) list list

val builtin : ctx -> string -> LangBuiltinBase.t array -> LangBuiltinBase.t

val register_builtins : LangTypeChecker.Environment.t -> LangTypeChecker.Environment.t

val initialise : ida_path:string -> (Elf.SectionHeader.t array * (Bap.Std.Addr.t -> bool) * Elf.StringTable.t * Bap.Std.image * Symtab.t * Core_kernel.Std.String.Set.t * Core_kernel.Std.String.Set.t) -> string -> ctx option
val clean_up   : ctx -> unit
