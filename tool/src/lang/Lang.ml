module Environment = LangTypeChecker.Environment

type environment = Environment.t LangUtil.M.t

let default_environment = LangUtil.M.empty

let load_profile env file =
  let env' = LangTypeChecker.type_check (LangBuiltin.register_builtins Environment.empty) @@ LangImport.file file in
  match Environment.lookup_rule "main" env' with
    | Some ((LangAst.TBOOL, [||]), _) ->
        LangUtil.M.add Filename.(basename file |> chop_extension) env' env
    | Some _ ->
        (* Error in type of main function *)
        invalid_arg "main function should be of type () -> bool"
    | None ->
        (* Main function not defined *)
        invalid_arg "main function undefined"

let evaluate_profile ~ida_path env name loader_state file =
  try
    let env = LangUtil.M.find name env in
    match Environment.lookup_rule "main" env with
      | None -> invalid_arg "profile does not exist"
      | Some (_, e) ->
          let ctx = match LangEval.Context.of_environment ~ida_path env loader_state file with
            | None -> failwith "unable to initialise evaluation context"
            | Some ctx -> ctx
          in
          let ret = match LangEval.eval ctx e with
            | LangBuiltinBase.BOOL b -> Some b
            | LangBuiltinBase.BOTTOM -> None
            | _ -> invalid_arg "evaluated profile to unexpected type"
          in
          LangEval.Context.clean_up ctx;
          ret
  with
    | Not_found -> None
