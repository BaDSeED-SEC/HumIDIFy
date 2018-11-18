open LangAst

module Environment = struct

  type t = {
    rules     : ((ty * (string * ty) array) * LangAst.expr) LangUtil.M.t;
    variables : ty LangUtil.M.t;
  }

  let empty = {
    rules     = LangUtil.M.empty;
    variables = LangUtil.M.empty;
  }

  let add_rule name ty e env =
    { env with rules = LangUtil.M.add name (ty, e) env.rules }

  let add_var name ty env =
    { env with variables = LangUtil.M.add name ty env.variables }

  let lookup k t = try Some (LangUtil.M.find k t) with Not_found -> None

  let mem_rule name env = LangUtil.M.mem name env.rules
  let mem_var name env = LangUtil.M.mem name env.variables

  let lookup_rule name env = lookup name env.rules
  let lookup_var name env = lookup name env.variables

end

let error_at { spos_line; spos_offset; epos_line; epos_offset } m =
  raise @@ LangUtil.Error (m, spos_line, spos_offset, epos_line, epos_offset)

let rec type_check env = function
  | [] -> env
  | (file, v) :: xs ->
      let (name, args, e) = v.value in
      if Environment.mem_rule name env then
        invalid_arg "redefinition of rule"
      else
        type_check (type_check_rule env name file args e) xs

and type_check_rule env name file args e =
  let (fty, env') = Array.fold_right (fun (name, ty) (fty', env'') ->
    (name, ty) :: fty', Environment.add_var name ty env''
  ) args ([], env) in
  let ty = type_check_expr env' e in
  Environment.add_rule name (ty, Array.of_list fty) e env

and type_check_expr env = function
  | BEXPR ({ value = (e1, op, e2) } as n) ->
      let ty1 = type_check_expr env e1 in
      let ty2 = type_check_expr env e2 in

      begin match ty1, op, ty2 with
        | TBOOL, (EQUAL | NEQUAL | LESS | GREATER | LESSEQ | GREATEREQ | LOGICAND | LOGICOR | LOGICXOR | SEMI), TBOOL -> TBOOL
        | TINT, (PLUS | MINUS | TIMES | DIVIDE | MOD | BITAND | BITOR | BITXOR | LSHIFT | RSHIFT), TINT -> TINT
        | TINT, (EQUAL | NEQUAL | LESS | GREATER | LESSEQ | GREATEREQ), TINT -> TBOOL
        | TSTRING, (EQUAL | NEQUAL), TSTRING -> TBOOL
        | TBOTTOM, (EQUAL | NEQUAL), (TBOOL | TINT | TSTRING)
        | (TBOOL | TINT | TSTRING), (EQUAL | NEQUAL), TBOTTOM -> TBOOL
        | _ -> error_at n "type mismatch for binary operator"
      end
  | UEXPR ({ value = (op, e) } as n) ->
      let ty = type_check_expr env e in

      begin match op, ty with
        | (PLUS | MINUS | BITNEG), TINT -> TINT
        | LOGICNEG, TBOOL -> TBOOL
        | _ -> error_at n "type mismatch for unary operator"
      end
  | EXISTS ({ value = (_, args, e) } as n) | FORALL ({ value = (_, args, e) } as n) ->
      let env' = Array.fold_left (fun env' v ->
        match v with
          | None -> env'
          | Some (name, ty) ->
              if Environment.mem_var name env' then
                error_at n (Printf.sprintf "attempting to shadow already bound variable `%s'" name)
              else
                Environment.add_var name ty env') env args
      in
      if type_check_expr env' e = TBOOL then
        TBOOL
      else
        error_at n "quantifier expression should evaluate to type bool"
  | IF ({ value = (econd, etrue, efalse) } as n) ->
      if type_check_expr env econd = TBOOL then
        let etruety = type_check_expr env etrue in
        let efalsety = type_check_expr env efalse in
        if etruety = efalsety then
          etruety
        else
          error_at n "type mismatch in arms of conditional expression"
      else
        error_at n "condition for conditional expression should evaluate to type bool"
  | LET ({ value = (name, e1, e2) } as n) ->
      let tyb = type_check_expr env e1 in
      if Environment.mem_var name env then
        error_at n (Printf.sprintf "attempting to shadow already bound variable `%s'" name)
      else
        type_check_expr (Environment.add_var name tyb env) e2
  | EVAL ({ value = (name, es) } as n) ->
      begin match Environment.lookup_rule name env with
        | Some ((ty, tys), _) ->
            if Array.length tys = Array.length es then (
              Array.iteri (fun i v ->
                if snd tys.(i) <> type_check_expr env v then
                  error_at n (Printf.sprintf "argument %d to rule `%s' is not of the expected type" i name)) es;
              ty
            ) else
              error_at n (Printf.sprintf "call to rule `%s' with incorrect number of arguments" name)
        | None -> error_at n (Printf.sprintf "rule `%s' is unbound" name)
      end
  | BOOL _ -> TBOOL
  | INT _ -> TINT
  | STRING _ -> TSTRING
  | BOTTOM _ -> TBOTTOM
  | VAR ({ value = name } as n) ->
      begin match Environment.lookup_var name env with
        | Some t -> t
        | None -> error_at n (Printf.sprintf "variable `%s' is unbound" name)
      end
  | BUILTIN -> invalid_arg "builtin node cannot be type checked"
