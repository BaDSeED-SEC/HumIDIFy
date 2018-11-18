open Core_kernel.Std
open Bap.Std

type args = string list

type _ arg =
  | ArgBoolean     : string * string -> (bool -> args) arg
  | ArgFlag        : string -> (bool -> args) arg
  | ArgFloat       : string * string -> (float -> args) arg
  | ArgInteger     : string * string -> (int -> args) arg
  | ArgString      : string * string -> (string -> args) arg

  | ArgAnonBoolean : (bool -> args) arg
  | ArgAnonFloat   : (float -> args) arg
  | ArgAnonInteger : (int -> args) arg
  | ArgAnonString  : (string -> args) arg

  | ArgOptBoolean  : string * string -> (bool option -> args) arg
  | ArgOptFlag     : string -> (bool option -> args) arg
  | ArgOptFloat    : string * string -> (float option -> args) arg
  | ArgOptInteger  : string * string -> (int option -> args) arg
  | ArgOptString   : string * string -> (string option -> args) arg

  | ArgCombine     : ('a -> args) arg * 'b arg -> ('a -> 'b) arg

let bool ?(sep = " ") n       = ArgBoolean (sep, n)
let flag n                    = ArgFlag n
let float ?(sep = " ") n      = ArgFloat (sep, n)
let int ?(sep = " ") n        = ArgInteger (sep, n)
let string ?(sep = " ") n     = ArgString (sep, n)

let anon_bool                 = ArgAnonBoolean
let anon_float                = ArgAnonFloat
let anon_int                  = ArgAnonInteger
let anon_string               = ArgAnonString

let opt_bool ?(sep = " ") n   = ArgOptBoolean (sep, n)
let opt_flag n                = ArgOptFlag n
let opt_float ?(sep = " ") n  = ArgOptFloat (sep, n)
let opt_int ?(sep = " ") n    = ArgOptInteger (sep, n)
let opt_string ?(sep = " ") n = ArgOptString (sep, n)

let (&) a b       = ArgCombine (a, b)

(* Prevent shell injection *)
let format_string str =
  let len = String.length str in
  let buf = Buffer.create len in
  Buffer.add_char buf '"';
  String.iter ~f:(function
    | '$' -> Buffer.add_string buf "\\$"
    | '`' -> Buffer.add_string buf "\\`"
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\""
    | c -> Buffer.add_char buf c)
    str;
  Buffer.add_char buf '"';
  Buffer.contents buf

let pp_boolean = function true -> "true" | false -> "false"
let pp_float = sprintf "%0f"
let pp_integer = string_of_int
let pp_string = format_string

let opt f sep n v xs = match v with
  | None -> xs
  | Some x -> f x :: " " :: n :: " " :: xs

(* Returns the argument list in reverse order *)
let args : type a. a arg -> a = fun t ->
  let rec aux : type a. a arg -> string list -> a = function
    | ArgBoolean (sep, n) -> fun xs b -> pp_boolean b :: sep :: n :: " " :: xs
    | ArgFlag n -> fun xs b -> if b then n :: " " :: xs else xs
    | ArgFloat (sep, n) -> fun xs f -> pp_float f :: sep :: n :: " " :: xs
    | ArgInteger (sep, n) -> fun xs i -> pp_integer i :: sep :: n :: " " :: xs
    | ArgString (sep, n) -> fun xs s -> pp_string s :: sep :: n :: " ":: xs

    | ArgAnonBoolean -> fun xs b -> pp_boolean b :: " " :: xs
    | ArgAnonFloat -> fun xs f -> pp_float f :: " " :: xs
    | ArgAnonInteger -> fun xs i -> pp_integer i :: " " :: xs
    | ArgAnonString -> fun xs s -> pp_string s :: " " :: xs

    | ArgOptBoolean (sep, n) -> fun xs b -> opt pp_boolean sep n b xs
    | ArgOptFlag n -> fun xs -> (function None | Some false -> xs | _ -> n :: " " :: xs)
    | ArgOptFloat (sep, n) -> fun xs f -> opt pp_float sep n f xs
    | ArgOptInteger (sep, n) -> fun xs i -> opt pp_integer sep n i xs
    | ArgOptString (sep, n) -> fun xs s -> opt pp_string sep n s xs

    | ArgCombine (a, b) -> fun xs a' -> aux b (aux a xs a')
  in
  aux t []

let run ?(env = []) n args =
  let env' = List.fold_right env ~init:[] ~f:(fun (k, v) xs -> k :: v :: xs)
           |> Array.of_list
  in
  Unix.open_process_full ((format_string n) ^ List.fold_left args ~init:"" ~f:(fun s x -> x ^ s)) env'
