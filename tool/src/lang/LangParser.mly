%{
  exception SyntaxError of int * int * int * int

  let syntax_error spos epos =
    raise @@ SyntaxError (spos.Lexing.pos_cnum - spos.Lexing.pos_bol, spos.Lexing.pos_lnum, epos.Lexing.pos_cnum - epos.Lexing.pos_bol, epos.Lexing.pos_lnum)
%}

(* Types *)
%token TYPE_BOOL TYPE_INT TYPE_STRING

(* Values *)
%token FALSE TRUE ERROR
%token <int64> INT
%token <string> STRING
%token <string> IDENT

(* Keywords *)
%token KWD_RULE KWD_IMPORT
%token KWD_LET KWD_IN
%token KWD_IF KWD_THEN KWD_ELSE
%token KWD_FORALL KWD_EXISTS

(* Misc. symbols *)
%token ASSIGN ARROW COMMA COLON LPAREN RPAREN UNDERSCORE

(* Operators *)
%token OP_PLUS OP_MINUS OP_TIMES OP_DIVIDE OP_MOD
%token OP_BITAND OP_BITOR OP_BITXOR OP_BITNEG OP_LSHIFT OP_RSHIFT
%token OP_EQUAL OP_NEQUAL OP_LESS OP_GREATER OP_LESSEQ OP_GREATEREQ
%token OP_LOGICAND OP_LOGICOR OP_LOGICXOR OP_SEMI OP_LOGICNEG

%token EOF

(* Operator precedence *)
%nonassoc OP_COND_LET

%left OP_LOGICOR
%left OP_LOGICXOR
%left OP_LOGICAND
%left OP_SEMI

%left OP_BITOR
%left OP_BITXOR
%left OP_BITAND

%left OP_EQUAL OP_NEQUAL
%left OP_LESS OP_GREATER OP_LESSEQ OP_GREATEREQ

%left OP_LSHIFT OP_RSHIFT

%left OP_PLUS OP_MINUS
%left OP_TIMES OP_DIVIDE OP_MOD

%nonassoc OP_ARITHPOS OP_ARITHNEG OP_BITNEG OP_LOGICNEG
%nonassoc OP_QUAL

%type  <LangAst.expr> expr
%type  <LangAst.node> tl_expr
%start <LangAst.t>    parse

%%

parse:
  | EOF                                                                                       { [] }
  | x = tl_expr xs = parse                                                                    { x :: xs }
  | error                                                                                     { syntax_error $startpos $endpos }
  ;

tl_expr:
  | KWD_IMPORT name = STRING                                                                  { LangAst.IMPORT (LangAst.node $startpos $endpos name) }
  | KWD_RULE name = IDENT LPAREN params = separated_list(COMMA, id_ty) RPAREN ASSIGN e = expr { LangAst.RULE (LangAst.node $startpos $endpos (name, Array.of_list params, e)) }
  ;

id_ty:
  | name = IDENT COLON TYPE_BOOL                                                              { (name, LangAst.TBOOL) }
  | name = IDENT COLON TYPE_INT                                                               { (name, LangAst.TINT) }
  | name = IDENT COLON TYPE_STRING                                                            { (name, LangAst.TSTRING) }

expr:
  | e1 = expr op = binary_op e2 = expr                                                        { LangAst.BEXPR (LangAst.node $startpos $endpos (e1, op, e2)) }
  | op = unary_op e = expr                                                                    { LangAst.UEXPR (LangAst.node $startpos $endpos (op, e)) }

  (* Special handling of dual binary/unary operators since menhir won't inline with %prec *)
  | OP_PLUS e = expr  %prec OP_ARITHPOS                                                       { LangAst.UEXPR (LangAst.node $startpos $endpos (LangAst.PLUS, e)) }
  | OP_MINUS e = expr %prec OP_ARITHNEG                                                       { LangAst.UEXPR (LangAst.node $startpos $endpos (LangAst.MINUS, e)) }
  (* ------------------------------------------------------------------------------------ *)

  | LPAREN e = expr RPAREN                                                                    { e }
  | KWD_EXISTS body = qual_body                                    %prec OP_QUAL              { LangAst.EXISTS (LangAst.node $startpos $endpos body) }
  | KWD_FORALL body = qual_body                                    %prec OP_QUAL              { LangAst.FORALL (LangAst.node $startpos $endpos body) }
  | KWD_IF cond = expr KWD_THEN tcase = expr KWD_ELSE fcase = expr %prec OP_COND_LET          { LangAst.IF (LangAst.node $startpos $endpos (cond, tcase, fcase)) }
  | KWD_LET binding = IDENT ASSIGN value = expr KWD_IN body = expr %prec OP_COND_LET          { LangAst.LET (LangAst.node $startpos $endpos (binding, value, body)) }
  | name = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN                             { LangAst.EVAL (LangAst.node $startpos $endpos (name, Array.of_list args)) }
  | v = value                                                                                 { v }
  ;

%inline binary_op:
  | OP_PLUS                                                                                   { LangAst.PLUS }
  | OP_MINUS                                                                                  { LangAst.MINUS }
  | OP_TIMES                                                                                  { LangAst.TIMES }
  | OP_DIVIDE                                                                                 { LangAst.DIVIDE }
  | OP_MOD                                                                                    { LangAst.MOD }
  | OP_BITAND                                                                                 { LangAst.BITAND }
  | OP_BITOR                                                                                  { LangAst.BITOR }
  | OP_BITXOR                                                                                 { LangAst.BITXOR }
  | OP_LSHIFT                                                                                 { LangAst.LSHIFT }
  | OP_RSHIFT                                                                                 { LangAst.RSHIFT }
  | OP_EQUAL                                                                                  { LangAst.EQUAL }
  | OP_NEQUAL                                                                                 { LangAst.NEQUAL }
  | OP_LESS                                                                                   { LangAst.LESS }
  | OP_GREATER                                                                                { LangAst.GREATER }
  | OP_LESSEQ                                                                                 { LangAst.LESSEQ }
  | OP_GREATEREQ                                                                              { LangAst.GREATEREQ }
  | OP_LOGICAND                                                                               { LangAst.LOGICAND }
  | OP_LOGICOR                                                                                { LangAst.LOGICOR }
  | OP_LOGICXOR                                                                               { LangAst.LOGICXOR }
  | OP_SEMI                                                                                   { LangAst.SEMI }
  ;

%inline unary_op:
  | OP_BITNEG                                                                                 { LangAst.BITNEG }
  | OP_LOGICNEG                                                                               { LangAst.LOGICNEG }
  ;

%inline qual_body:
  | name = IDENT LPAREN args = separated_list(COMMA, id_ty_) RPAREN ARROW e = expr            { (name, Array.of_list args, e) }
  ;

id_ty_:
  | name = IDENT COLON TYPE_BOOL                                                              { Some (name, LangAst.TBOOL) }
  | name = IDENT COLON TYPE_INT                                                               { Some (name, LangAst.TINT) }
  | name = IDENT COLON TYPE_STRING                                                            { Some (name, LangAst.TSTRING) }
  | UNDERSCORE                                                                                { None }
  ;

%inline value:
  | FALSE                                                                                     { LangAst.BOOL (LangAst.node $startpos $endpos false) }
  | TRUE                                                                                      { LangAst.BOOL (LangAst.node $startpos $endpos true) }
  | ERROR                                                                                     { LangAst.BOTTOM (LangAst.node $startpos $endpos ()) }
  | v = INT                                                                                   { LangAst.INT (LangAst.node $startpos $endpos v) }
  | v = STRING                                                                                { LangAst.STRING (LangAst.node $startpos $endpos v) }
  | v = IDENT                                                                                 { LangAst.VAR (LangAst.node $startpos $endpos v) }
  ;
