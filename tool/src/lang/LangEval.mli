module Context : sig

  type t

  val of_environment : ida_path:string -> LangTypeChecker.Environment.t -> (Elf.SectionHeader.t array * (Bap.Std.Addr.t -> bool) * Elf.StringTable.t * Bap.Std.image * Symtab.t * Core_kernel.Std.String.Set.t * Core_kernel.Std.String.Set.t) -> string -> t option
  val clean_up       : t -> unit

  val add_var        : string -> LangBuiltinBase.t -> t -> t
  val lookup_rule    : string -> t -> ((LangAst.ty * (string * LangAst.ty) array) * LangAst.expr)
  val lookup_var     : string -> t -> LangBuiltinBase.t

  val freshen        : t -> t

end

val eval             : Context.t -> LangAst.expr -> LangBuiltinBase.t
