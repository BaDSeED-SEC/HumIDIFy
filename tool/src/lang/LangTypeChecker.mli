module Environment : sig

  type t = {
    rules     : ((LangAst.ty * (string * LangAst.ty) array) * LangAst.expr) LangUtil.M.t;
    variables : LangAst.ty LangUtil.M.t;
  }

  val empty       : t

  val add_rule    : string -> (LangAst.ty * (string * LangAst.ty) array) -> LangAst.expr -> t -> t
  val add_var     : string -> LangAst.ty -> t -> t

  val lookup_rule : string -> t -> ((LangAst.ty * (string * LangAst.ty) array) * LangAst.expr) option
  val lookup_var  : string -> t -> LangAst.ty option
end

val type_check    : Environment.t -> LangImport.t -> Environment.t
