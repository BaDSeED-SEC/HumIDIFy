module Ast = LangAst

module T = struct
  type t = string

  let compare = String.compare
end

module I = struct
  type t = int

  let compare = compare
end

module IntM    = Map.Make (I)
module M       = Map.Make (T)
module S       = Set.Make (T)
module DigestS = Set.Make (Digest)
module DigestM = struct
  type t = DigestS.t M.t

  let empty = M.empty

  let mem label digest t =
    try DigestS.mem digest @@ M.find label t with
      | Not_found -> false

  let add label digest t =
    try
      let digests = M.find label t in
      M.add label (DigestS.add digest digests) t
    with
      | Not_found -> M.add label (DigestS.singleton digest) t

  let iter = M.iter
end

module Option = struct

  let bind v f  = match v with Some v -> f v | _ -> None
  let return v  = Some v

  let (>>=)     = bind
  let (>>|) v f = bind v (fun v' -> Some (f v'))

  let map ~default f = function None -> default | Some v -> f v
  let value = map (fun x -> x)
  let value_exn = function None -> failwith "value_exn: expected value" | Some v -> v

end

module List = struct
  include List

  let rec take n xs = match xs with
    | _ when n <= 0 -> []
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs

  let rec drop_while f = function
    | x :: xs when f x -> drop_while f xs
    | xs -> xs

  let rec take_while f = function
    | x :: xs when f x -> x :: take_while f xs
    | _ -> []

  let rec fold_while f v = function
    | [] -> v
    | x :: xs ->
        match f v x with
          | `Ok v' -> fold_while f v' xs
          | `Stop v' -> v'
end

module String = struct

  include String

  let filter f str =
    let buf = Buffer.create (String.length str) in
    String.iter (fun c -> if f c then Buffer.add_char buf c) str;
    Buffer.contents buf

  let drop_prefix str n =
    let len = String.length str in
    let buf = Buffer.create len in
    for i = n to len - 1 do
      Buffer.add_char buf str.[i]
    done;
    Buffer.contents buf

end

module Log = struct

  let pr_error ?(code = 1) str =
    prerr_endline @@ "[\027[1;31m!!\027[0m] " ^ str;
    exit code

  let pr_warn ?(verbose = false) str =
    if verbose then prerr_endline @@ "[\027[1;33m*!\027[0m] " ^ str

  let pr_info ?(verbose = false) str =
    if verbose then prerr_endline @@ "[**] " ^ str

  let pr str =
    print_endline @@ "[**] " ^ str

  let pr_bold str =
    print_endline @@ "[\027[1m**\027[0m] " ^ str

end

let input_lines file =
  let ic = open_in file in
  let rec aux acc =
    match try Some (input_line ic) with End_of_file -> None with
      | None -> List.rev acc
      | Some line -> aux (line :: acc)
  in
  aux []

let with_in_file ?(binary = false) f file =
  let ic = if binary then open_in_bin file else open_in file in
  let res =
    try
      f ic
    with
      | exn ->
          close_in_noerr ic;
          raise exn
  in
  close_in_noerr ic;
  res

exception Error of string * int * int * int * int

let parse_from lb =
  LangParser.parse LangLexer.token lb

let parse_string s   = parse_from (Lexing.from_string s)
let parse_channel c  = parse_from (Lexing.from_channel c)
let parse_function f = parse_from (Lexing.from_function f)

let parse_file file =
  let ic = open_in file in
  let ast = parse_channel ic in
  close_in_noerr ic;
  ast
