type oper =
  | PLUS
  | MINUS

  | TIMES
  | DIVIDE
  | MOD

  | BITAND
  | BITOR
  | BITXOR
  | BITNEG
  | LSHIFT
  | RSHIFT

  | EQUAL
  | NEQUAL
  | LESS
  | GREATER
  | LESSEQ
  | GREATEREQ

  | LOGICAND
  | LOGICOR
  | LOGICXOR
  | LOGICNEG
  | SEMI

type ty =
  | TBOOL
  | TINT
  | TSTRING
  | TBOTTOM

type 'a node_content = {
  spos_line   : int;
  spos_offset : int;
  epos_line   : int;
  epos_offset : int;
  value       : 'a;
}

type expr =
  | BEXPR of (expr * oper * expr) node_content
  | UEXPR of (oper * expr) node_content
  | EXISTS of (string * (string * ty) option array * expr) node_content
  | FORALL of (string * (string * ty) option array * expr) node_content
  | IF of (expr * expr * expr) node_content
  | LET of (string * expr * expr) node_content
  | EVAL of (string * expr array) node_content
  | BOOL of bool node_content
  | INT of int64 node_content
  | STRING of string node_content
  | BOTTOM of unit node_content
  | VAR of string node_content
  | BUILTIN

type node =
  | IMPORT of string node_content
  (* Invariant: parameters = setify(parameters) *)
  | RULE of (string * (string * ty) array * expr) node_content

type t = node list

val node      : Lexing.position -> Lexing.position -> 'a -> 'a node_content
