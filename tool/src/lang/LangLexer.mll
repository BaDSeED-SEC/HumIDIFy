{
  open Printf
  open LangParser

  exception Eof
  exception SyntaxError of int * int * string

  let string_buf = Buffer.create 256

  let syntax_error lexbuf message =
    let pos = lexbuf.Lexing.lex_curr_p in
    raise @@ SyntaxError (pos.Lexing.pos_cnum - pos.Lexing.pos_bol, pos.Lexing.pos_lnum, message)

  let add_char = Buffer.add_char

  let add_escape buf v =
    Buffer.add_char buf begin match v with
      | 'a' -> '\x07'
      | 'b' -> '\x08'
      | 'f' -> '\x0c'
      | 'n' -> '\x0a'
      | 'r' -> '\x0d'
      | 't' -> '\x09'
      | 'v' -> '\x0b'
      | '\\' -> '\x5c'
      | '\'' -> '\x27'
      | '"' -> '\x22'
      (* Disregard the escape qualifier if not a real escaped character *)
      | c -> c
    end

  let intn_of_string lexbuf base str =
    let intn = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
    let base' = Int64.of_int base in
    let len = String.length str in

    if base < 1 || base > String.length intn then
      invalid_arg "intn_of_string: invalid base";

    try
      let rec aux acc idx =
        if idx < len then
          let v = Char.uppercase str.[idx]
                |> String.index intn
                |> Int64.of_int
          in
          aux Int64.(add v (mul acc base')) (idx + 1)
        else
          acc
      in aux 0L 0
    with
      | Not_found -> syntax_error lexbuf "invalid integer literal"

  let add_hex buf v =
    Buffer.add_char buf (
      Char.chr @@ int_of_string @@ "0x" ^ v
    )

  let lookup_ident =
    let keywords = Hashtbl.create 8 in
    let keyword_map = [
      "rule", KWD_RULE;
      "import", KWD_IMPORT;
      "let", KWD_LET;
      "in", KWD_IN;
      "if", KWD_IF;
      "then", KWD_THEN;
      "else", KWD_ELSE;
      "forall", KWD_FORALL;
      "exists", KWD_EXISTS;
    ] in
    List.iter (fun (name, token) -> Hashtbl.add keywords name token) keyword_map;
    fun err ident ->
      try
        Hashtbl.find keywords ident
      with
        | Not_found -> IDENT ident

  let lookup_operator =
    let operators = Hashtbl.create 20 in
    let operator_map = [
      "+", OP_PLUS;
      "-", OP_MINUS;
      "*", OP_TIMES;
      "/", OP_DIVIDE;
      "%", OP_MOD;
      "&", OP_BITAND;
      "|", OP_BITOR;
      "^", OP_BITXOR;
      "~", OP_BITNEG;
      "<<", OP_LSHIFT;
      ">>", OP_RSHIFT;
      "==", OP_EQUAL;
      "!=", OP_NEQUAL;
      "<", OP_LESS;
      ">", OP_GREATER;
      "<=", OP_LESSEQ;
      ">=", OP_GREATEREQ;
      "&&", OP_LOGICAND;
      "||", OP_LOGICOR;
      "^^", OP_LOGICXOR;
      "!", OP_LOGICNEG;
      ";", OP_SEMI;
    ] in
    List.iter (fun (name, token) -> Hashtbl.add operators name token) operator_map;
    fun lexbuf operator ->
      try
        Hashtbl.find operators operator
      with
        | Not_found ->
            syntax_error lexbuf @@ sprintf "unknown operator `%s'" operator

}


let whitespace = [' ''\t']
let newline    = '\n' | "\r\n"

let identifier = ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_']*

let decdigit   = ['0'-'9']
let hexdigit   = ['A'-'F''a'-'f''0'-'9']
let octdigit   = ['0'-'7']
let bindigit   = ['0''1']

let operator   = ['+''-''*''/''%''&''|''^''~''<''>''!'';']
               | "<<" | ">>" | "==" | "!=" | "<=" | ">="
               | "&&" | "||" | "^^"

let esc_char   = ['a''b''f''n''r''t''v''\\''\'''"']
let str_char   = [' ''!''#'-'&''('-'['']'-'~']


rule token = parse
  | whitespace+                      { token lexbuf }
  | newline                          { Lexing.new_line lexbuf; token lexbuf }

  (* Comment *)
  | "--" [^'\n']*                    { token lexbuf }

  (* Types *)
  | "bool"                           { TYPE_BOOL }
  | "int"                            { TYPE_INT }
  | "string"                         { TYPE_STRING } 

  (* Constants *)
  | "false"                          { FALSE }
  | "true"                           { TRUE }

  (* Unsure of this token; I'd like something beautiful... *)
  | "_|_"                            { ERROR }

  | '0' ['b''B'] (bindigit+ as v)    { INT (intn_of_string lexbuf 2 v) }
  | ('0' ['d''D'])? (decdigit+ as v) { INT (intn_of_string lexbuf 10 v) }
  | '0' ['o''O'] (octdigit+ as v)    { INT (intn_of_string lexbuf 8 v) }
  | '0' ['x''X'] (hexdigit+ as v)    { INT (intn_of_string lexbuf 16 v) }

  | '"'                              { token_string lexbuf }

  (* Misc., non-alphanumeric keywords *)
  | ','                              { COMMA }
  | '('                              { LPAREN }
  | ')'                              { RPAREN }
  | "=>"                             { ARROW }
  | '='                              { ASSIGN }
  | ':'                              { COLON }
  | '_'                              { UNDERSCORE }

  (* Operators *)
  | operator as v                    { lookup_operator lexbuf v }

  (* Identifiers/keywords *)
  | identifier as v                  { lookup_ident lexbuf v }

  | eof                              { EOF }


and token_string = parse
  | '"'                               {
    let str = STRING (Buffer.contents string_buf) in
    (* Save allocation of a new buffer for each string literal *)
    Buffer.clear string_buf;
    str
  }
  | '\\' 'x' (hexdigit hexdigit as v) {
    add_hex string_buf v;
    token_string lexbuf
  }
  | '\\' (esc_char | str_char as c)   {
    add_escape string_buf c;
    token_string lexbuf
  }
  | str_char as c                     {
    add_char string_buf c;
    token_string lexbuf
  }
  | _ as c                            {
    syntax_error lexbuf @@ sprintf "invalid character in string literal `%c'" c
  }
