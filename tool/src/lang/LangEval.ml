open LangAst
open LangBuiltin
open LangBuiltinBase
open LangTypeChecker
open LangUtil.Option

module Context = struct

  open LangTypeChecker

  type t = {
    file      : string;
    state     : LangBuiltin.ctx;
    rules     : ((LangAst.ty * (string * LangAst.ty) array) * LangAst.expr) LangUtil.M.t;
    variables : LangBuiltinBase.t LangUtil.M.t;
  }

  let of_environment ~ida_path { Environment.rules = rules } loader_state file =
    LangBuiltin.initialise ~ida_path loader_state file >>| fun state -> {
        file      = file;
        state     = state;
        rules     = rules;
        variables = LangUtil.M.empty;
  }

  let clean_up { state } =
    LangBuiltin.clean_up state

  let freshen t = { t with variables = LangUtil.M.empty }

  let add_var name v t =
    { t with variables = LangUtil.M.add name v t.variables }

  (*
     No need for lookup of the rule/var name at this stage: if it type checked,
     then we have knowledge of the existence of it
   *)
  let lookup_rule name t = LangUtil.M.find name t.rules
  let lookup_var name t = LangUtil.M.find name t.variables

end

let error_at { spos_line; spos_offset; epos_line; epos_offset } m =
  raise @@ LangUtil.Error (m, spos_line, spos_offset, epos_line, epos_offset)

let lift_b2 f a b = BOOL (f a b)

let bexpr_b2 op = lift_b2 ((function
  | EQUAL -> (=)
  | NEQUAL -> (<>)
  | LESS -> (<)
  | LESSEQ -> (<=)
  | GREATER -> (>)
  | GREATEREQ -> (>=)
  | LOGICAND -> (&&)
  | LOGICOR -> (||)
  | LOGICXOR -> fun a b -> ((a || b) && not (a && b))
  | SEMI -> fun _ b -> b
  | _ -> invalid_arg "operator undefined for bool * bool") op)

let lift_i2 f a b = INT (f a b)

let bexpr_i2 = function
  | PLUS -> lift_i2 Int64.add
  | MINUS -> lift_i2 Int64.sub
  | TIMES -> lift_i2 Int64.mul
  | DIVIDE -> lift_i2 Int64.div
  | MOD -> lift_i2 Int64.rem
  | BITAND -> lift_i2 Int64.logand
  | BITOR -> lift_i2 Int64.logor
  | BITXOR -> lift_i2 Int64.logxor
  | LSHIFT -> lift_i2 (fun a b -> Int64.shift_left a (Int64.to_int @@ min 64L b))
  | RSHIFT -> lift_i2 (fun a b -> Int64.shift_right_logical a (Int64.to_int @@ min 64L b))
  | EQUAL -> lift_b2 (=)
  | NEQUAL -> lift_b2 (<>)
  | LESS -> lift_b2 (<)
  | LESSEQ -> lift_b2 (<=)
  | GREATER -> lift_b2 (>)
  | GREATEREQ -> lift_b2 (>=)
  | _ -> invalid_arg "operator undefined for int * int"

let lift_s2 f a b = BOOL (f a b)

let bexpr_s2 = function
  | EQUAL -> lift_s2 (=)
  | NEQUAL -> lift_s2 (<>)
  | _ -> invalid_arg "operator undefined for string * string"

let lift_b1 f a = BOOL (f a)

let uexpr_b = function
  | LOGICNEG -> lift_b1 not
  | _ -> invalid_arg "operator not defined for bool"

let lift_i1 f a = INT (f a)

let uexpr_i = function
  | BITNEG -> lift_i1 Int64.lognot
  | PLUS -> lift_i1 (fun x -> x)
  | MINUS -> lift_i1 Int64.neg
  | _ -> invalid_arg "operator not defined for int"

let rec quantifier env f name args e = match f name args with
  | None -> BOTTOM
  | Some es ->
      let env' = Array.fold_left (fun env' -> function None -> env' | Some (name, v) ->
        Context.add_var name v env')
        env
        es
      in
      eval env' e

and  eval env = function
  | BEXPR ({ value = (e1, op, e2) } as n) ->
      let e1' = eval env e1 in
      (* FIXME: Bad eager evaluation that can cause side-effects. *)
      let e2' = eval env e2 in

      begin match e1', op, e2' with
        | BOOL b1, (EQUAL | NEQUAL | LESS | GREATER | LESSEQ | GREATEREQ | LOGICAND | LOGICOR | LOGICXOR | SEMI as op), BOOL b2 ->
            bexpr_b2 op b1 b2
        | INT i1, (PLUS | MINUS | TIMES | DIVIDE | MOD | BITAND | BITOR | BITXOR | LSHIFT | RSHIFT as op), INT i2
        | INT i1, (EQUAL | NEQUAL | LESS | GREATER | LESSEQ | GREATEREQ as op), INT i2 ->
            bexpr_i2 op i1 i2
        | STRING s1, (EQUAL | NEQUAL as op), STRING s2 ->
            bexpr_s2 op s1 s2
        | BOTTOM, (EQUAL | NEQUAL), (BOOL _ | INT _ | STRING _)
        | (BOOL _ | INT _ | STRING _), (EQUAL | NEQUAL), BOTTOM -> BOOL false

        | BOTTOM, EQUAL, BOTTOM -> BOOL true
        | BOTTOM, NEQUAL, BOTTOM -> BOOL false

        | _ -> error_at n "type mismatch for binary operator in expression"
      end
  | UEXPR ({ value = (op, e) } as n) ->
      let e' = eval env e in

      begin match op, e' with
        | (PLUS | MINUS | BITNEG as op), INT i -> uexpr_i op i
        | LOGICNEG as op, BOOL b -> uexpr_b op b
        | _ -> error_at n "type mismatch for unary operator in expression"
      end
  | EXISTS { value = (name, args, e) } ->
      BOOL (List.exists (fun xs ->
        let env' = List.fold_left (fun env' (name, v) ->
          Context.add_var name v env')
          env
          xs
        in
        BOOL true = eval env' e)
        (LangBuiltin.function_args env.Context.state name args))
  | FORALL { value = (name, args, e) } ->
      BOOL (List.for_all (fun xs ->
        let env' = List.fold_left (fun env' (name', v) ->
          Context.add_var name' v env')
          env
          xs
        in
        BOOL true = eval env' e)
        (LangBuiltin.function_args env.Context.state name args))
  | IF { value = (econd, etrue, efalse) } ->
      if eval env econd = BOOL true then
        eval env etrue
      else
        eval env efalse
  | LET { value = (name, e1, e2) } ->
      let e1' = eval env e1 in
      eval (Context.add_var name e1' env) e2
  | EVAL { value = (name, es) } ->
      let ((_, tys), e) = Context.lookup_rule name env in
      begin match e with
        | BUILTIN ->
            LangBuiltin.builtin env.Context.state
                                name
                                (Array.map (fun e' -> eval env e') es)
        | _ ->
            let env' =
              Array.fold_left (fun env' (name', e') ->
                Context.add_var name' (eval env e') env')
                (Context.freshen env)
                (Array.mapi (fun i e -> (fst tys.(i), e)) es)
            in
            eval env' e
      end
  | BOOL { value = b } -> BOOL b
  | INT { value = i } -> INT i
  | STRING { value = s } -> STRING s
  | BOTTOM _ -> BOTTOM
  | VAR { value = name } -> Context.lookup_var name env
  | BUILTIN -> invalid_arg "cannot evaluate raw builtin as expression"
