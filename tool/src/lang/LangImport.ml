open LangAst

module T = struct
  type t = Digest.t

  let compare = Digest.compare
end

module S = struct
  include Set.Make (T)

  let include_paths =
    try
      "" :: List.filter (function "" | "." -> false | _ -> true)
           (Str.split_delim Str.(regexp ":") @@ Sys.getenv "BFDL_INCLUDE")
    with
      | Not_found -> [""]

  let add_module env key = add key env
  let get_module_key env file =
    let path =
      let rec aux = function
        | [] -> failwith @@ "cannot import: " ^ file
        | x :: xs ->
            let path = Filename.concat x file in
            if Sys.file_exists path then
              path
            else
              aux xs
      in aux include_paths
    in
    let key = Digest.file path in
    if mem key env then
      None
    else
      Some (path, key)
end

type rule = string * (string * (string * ty) array * expr) node_content

type t = rule list

(* FIXME: Not tail-recursive... maybe in another lifetime. *)
let rec resolve name env = function
  | [] -> (env, [])
  | RULE v :: xs ->
      let (env', xs') = resolve name env xs in
      (env', (name, v) :: xs')
  | IMPORT { value = v } :: xs -> match S.get_module_key env v with
      | None -> resolve name env xs
      | Some (path, key) ->
          let (env', xs') = resolve path (S.add_module env key) (LangUtil.parse_file path) in
          let (env'', xs'') = resolve name env' xs in
          (env'', xs' @ xs'')

let file file = snd @@ resolve file S.empty (LangUtil.parse_file file)
