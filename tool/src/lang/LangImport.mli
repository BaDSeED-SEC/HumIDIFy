type rule = string * (string * (string * LangAst.ty) array * LangAst.expr) LangAst.node_content

type t = rule list

val file : string -> t
