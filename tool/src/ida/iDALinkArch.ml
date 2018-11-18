open Core_kernel.Std
open Bap.Std

module Arm = struct

  module Target = ARM

  let default_retn = Target.CPU.r0

  let resolve_pc mem =
    Bil.map (object
      inherit Bil.mapper as super

      method! map_var var =
        if Target.CPU.is_pc var then
          Bil.int @@ Target.CPU.addr_of_pc mem
        else
          super#map_var var
    end)

  let fold_constants =
    Bil.fixpoint @@ List.reduce_exn ~f:Fn.compose @@ List.map ~f:Bil.fixpoint [Bil.fold_consts]

  let resolve_indirects memory =
    Bil.map (object
      inherit Bil.mapper as super

      method! map_load ~mem ~addr endian scale =
        let expr = super#map_load ~mem ~addr endian scale in
        match addr with
          | Bil.Int addr ->
              begin Memmap.lookup memory addr |> Seq.hd |> function
                | None -> expr
                | Some (mem, _) -> match Memory.get ~scale ~addr mem with
                  | Ok word -> Bil.int word
                  | _ -> expr
              end
          | _ -> expr
    end)

  let resolve_calls ~memory ~bound insns =
    let aux mem stmt =
      let stmt' = resolve_pc mem [stmt]
               |> fold_constants
               |> resolve_indirects memory
               |> List.hd_exn
      in
      Stmt.find (object (self)
        inherit [addr] Bil.finder

        method! enter_jmp exp cc =
          match exp with
            (* Just handle the basic case for now *)
            | Bil.Int w ->
                let addr = Addr.of_int64 ~width:(Word.bitwidth w)
                                         (Or_error.ok_exn @@ Word.to_int64 w)
                in
                if not (bound addr) then
                  cc.return (Some addr)
                else
                  cc
            | _ -> cc
      end) stmt'
    in List.concat_map ~f:(fun (mem, insn) ->
      List.filter_map ~f:(fun stmt -> aux mem stmt) (Insn.bil insn)) insns

(*
  let influenced_by_call symbols memory block_graph g_block =
    if is_branching block_graph g_block then match List.rev @@ GroupBlock.to_blocks g_block with
      | term :: call :: _ ->
          let stmts = List.concat_map (Block.insns term) ~f:(fun (mem, insn) ->
            List.map (Insn.bil insn) ~f:(fun stmt ->
              Arm.resolve_pc mem [stmt] |>
              Arm.fold_constants |>
              Arm.resolve_indirects memory |>
              List.hd_exn))
          in

          let retn = Var.name ARM.CPU.r0 in

          (* Assume that R0 is the return address: we perform reaching definitions
             analysis.
           *)
          let get_expr_deps vars' expr =
            let gen = Exp.fold ~init:String.Set.empty (object
              inherit [String.Set.t] Bil.visitor as super

              method! visit_var var acc = Set.add acc (Var.name var)

            end) expr
            in
            List.map (Set.to_list gen) ~f:(fun name ->
              match Map.find vars' name with
                | None -> (name, 0)
                | Some vars'' ->
                    let (n, _) = Option.value_exn (List.max_elt ~cmp:(fun (n1, _) (n2, _) -> compare n1 n2)
                                                                vars'')
                    in
                    (name, n))
          in

          let rec aux vars = function
            | Bil.Move (var, expr) :: exprs ->
                (* FIXME: This *will* miss moves to/from memory. *)
                let deps = get_expr_deps vars expr in

                let name = Var.name var in
                (match Map.find vars name with
                  | None -> aux (Map.add_multi ~key:name ~data:(0, deps) vars) exprs
                  | Some vars' ->
                      let (n, _) =
                        Option.value_exn (List.max_elt ~cmp:(fun (n1, _) (n2, _) -> compare n1 n2) vars')
                      in
                      aux (Map.add_multi ~key:name ~data:(succ n, deps) vars) exprs)
            | [Bil.If (exp, _, _)] ->
                let to_process = Queue.of_list @@ get_expr_deps vars exp in
                let rec aux seen =
                  if Queue.length to_process = 0 then
                    None
                  else
                    let (name, n) as top = Queue.dequeue_exn to_process inn



                    if name = retn && n = 0 then
                      call_name symbols call
                    else
                      let seen' = top :: seen in
                      match Map.find vars name with
                        | None -> None
                        | Some deps' ->
                            let rec aux' = function
                              | [] -> aux seen'
                              | (n', deps'') :: xs ->
                                  if n = n' then (
                                    List.iter deps'' ~f:(fun dep -> if not (List.mem seen' dep) then Queue.enqueue to_process dep);
                                    aux seen'
                                  ) elsen


                                    aux' xs
                            in
                            aux' deps'
                in aux []
            | _ :: exprs ->
                aux vars exprs
            | [] -> None
          in

          aux (String.Map.singleton retn [(0, [])]) stmts
      | _ -> None
    else
      None
*)

  (* type ftab = [ `Fixed of var list Addr.Table.t | `Heuristic of var list ] *)

  let find_dependent_call ~memory ~bound ~ftab insns =

    let find_call mem stmt =
      let stmt' = resolve_pc mem [stmt]
               |> fold_constants
               |> resolve_indirects memory
               |> List.hd_exn
      in
      Stmt.find (object (self)
        inherit [addr] Bil.finder

        method! enter_jmp exp cc =
          match exp with
            (* Just handle the basic case for now *)
            | Bil.Int w ->
                let addr = Addr.of_int64 ~width:(Word.bitwidth w)
                                         (Or_error.ok_exn @@ Word.to_int64 w)
                in
                if not (bound addr) then
                  cc.return (Some addr)
                else
                  cc
            | _ -> cc
      end) stmt'
    in

    let rec process_insns influenced = function
      | [] -> None
      | (mem, stmt) :: insns ->
          match find_call mem stmt with
            | None -> process_insns (stmt :: influenced) insns
            | Some addr ->
                let m = match ftab with
                  | `Fixed ftab' -> Addr.Table.find ftab' addr
                  | `Heuristic m -> Some m
                in
                match m with
                  | None -> None
                  | Some arg_map ->
                      let call_info =
                        (* No upward tracing of call arguments *)
                        let number_of_args = List.length arg_map in
                        if number_of_args = 0 then
                          Some (addr, Var.Map.empty)
                        else
                          let args = List.fold insns ~init:Var.Map.empty ~f:(fun args' (mem, stmt) ->
                            let stmt' = resolve_pc mem [stmt]
                                     |> fold_constants
                                     |> resolve_indirects memory
                                     |> List.hd_exn
                            in
                            match stmt' with
                              | Bil.Move (var, expr) ->

                                  if Map.length args' < number_of_args
                                  && List.mem ~equal:Var.equal arg_map var
                                  && not @@ Map.mem args' var
                                  then
                                    (* Attempt to resolve exprs that aren't just a Bil.Int _ *)
                                    let expr = match expr with
                                      | Bil.Int _ -> expr
                                      | expr' ->
                                          let fvs = Exp.free_vars expr' in

                                          let sub_var expr var expr' =
                                            let exp = Exp.map
                                              (object
                                                inherit Bil.mapper as super

                                                method! map_var v =
                                                  if Var.name v = Var.name var then
                                                    expr'
                                                  else
                                                    super#map_var v
                                              end)
                                              expr
                                            in Exp.fold_consts exp
                                          in

                                          let rec find_fv_deps fvs expr insns =
                                            Set.fold_until fvs ~init:expr ~f:(fun expr' var ->
                                              let rec find_dep = function
                                                | (_, Bil.Move (var', exp)) :: insns when Var.name var' = Var.name var ->
                                                   (match exp with
                                                      | Bil.Int _ -> `Continue (sub_var expr' var exp)
                                                       | _ ->
                                                           match find_fv_deps (Exp.free_vars exp) exp insns with
                                                             | Bil.Int _ as exp' -> `Continue (sub_var expr' var exp')
                                                             | _ -> `Stop expr')
                                                | _ :: insns -> find_dep insns
                                                | [] -> `Stop expr'
                                              in
                                              find_dep insns)
                                          in

                                          find_fv_deps fvs expr' insns
                                    in
                                    Map.add args' ~key:var ~data:(mem, expr)
                                  else
                                    args'
                                | _ ->
                                    args')
                          in Some (addr, args)
                      in

                      (* Attempt to determine if the block branching is dependent upon r0 *)
                      let get_expr_deps vars' expr =
                        let gen = Exp.fold ~init:String.Set.empty (object
                          inherit [String.Set.t] Bil.visitor as super

                          method! visit_var var acc = Set.add acc (Var.name var)

                        end) expr
                        in
                        List.map (Set.to_list gen) ~f:(fun name ->
                          match Map.find vars' name with
                            | None -> (name, 0)
                            | Some vars'' ->
                                let (n, _) = Option.value_exn (List.max_elt ~cmp:(fun (n1, _) (n2, _) -> compare n1 n2)
                                                                            vars'')
                                in
                                (name, n))
                      in

                      let default_retn = Var.name default_retn in

                      let rec find_influence vars = function
                        | Bil.Move (var, expr) :: stmts ->
                            (* FIXME: This *will* miss moves to/from memory. *)
                            let deps = get_expr_deps vars expr in

                            let name = Var.name var in
                            (match Map.find vars name with
                              | None -> find_influence (Map.add_multi ~key:name ~data:(0, deps) vars) stmts
                              | Some vars' ->
                                  let (n, _) =
                                    Option.value_exn (List.max_elt ~cmp:(fun (n1, _) (n2, _) -> compare n1 n2) vars')
                                  in
                                  find_influence (Map.add_multi ~key:name ~data:(succ n, deps) vars) stmts)
                        | [Bil.If (exp, _, _)] ->
                            let to_process = Queue.of_list @@ get_expr_deps vars exp in
                            let rec aux seen =
                              if Queue.length to_process = 0 then
                                false
                              else
                                let (name, n) as top = Queue.dequeue_exn to_process in

                                if name = default_retn && n = 0 then
                                  true
                                else
                                  let seen' = top :: seen in
                                  match Map.find vars name with
                                    | None -> false
                                    | Some deps' ->
                                        let rec aux' = function
                                          | [] -> aux seen'
                                          | (n', deps'') :: xs ->
                                              if n = n' then (
                                                List.iter deps'' ~f:(fun dep -> if not (List.mem seen' dep) then Queue.enqueue to_process dep);
                                                aux seen'
                                              ) else
                                                aux' xs
                                        in
                                        aux' deps'
                            in aux []
                        | _ :: stmts ->
                            find_influence vars stmts
                        | [] -> false
                      in

                      let infl = find_influence (String.Map.singleton default_retn [(0, [])]) influenced in

                      if infl then
                        call_info
                      else
                        None
        in
        let bil = List.rev @@ List.concat_map insns ~f:(fun (mem, insns') ->
          List.cartesian_product [mem] (Insn.bil insns'))
        in

        let (mems, bils) = List.unzip bil in
        process_insns [] (List.zip_exn mems (fold_constants bils))

  
  let zcmp_dest ~memory ~dest1 ~dest2 insns =
    let insns = 
      List.rev @@ List.concat_map insns ~f:(fun (mem, insns') -> List.cartesian_product [mem] (Insn.bil insns'))
    in

    let open Option in
     List.hd insns >>= fun (mem, stmt) ->
       match stmt with
         | Bil.If (cond, jmps, []) ->
             let fvs = Exp.free_vars cond in
             if Set.mem fvs ARM.CPU.zf && Set.length fvs = 1 then
               List.tl insns >>= List.find ~f:(function (_, Bil.Move (zf', exp)) -> ARM.CPU.is_zf zf' | _ -> false) >>= function
                 | (_, Bil.Move (_, Bil.BinOp (op, exp1, exp2))) when op = Bil.EQ || op = Bil.NEQ ->
                     begin match exp1, exp2 with
                       | Bil.Int w, _ | _, Bil.Int w when Word.is_zero w ->
                          let cond = Exp.fold_consts cond in
                          if match cond with
                            | Bil.Var v | Bil.UnOp (Bil.NOT, Bil.Var v) -> ARM.CPU.is_zf v
                            | _ -> false
                          then
                            List.last jmps >>= fun jmp ->
                              let stmt' = resolve_pc mem [jmp]
                                       |> fold_constants
                                       |> resolve_indirects memory
                                       |> List.hd_exn
                              in
                              match stmt' with
                                | Bil.Jmp (Bil.Int dst) ->
                                    let addr = Addr.of_int64 ~width:(Addr.bitwidth dest1) @@ Or_error.ok_exn @@ Word.to_int64 dst in
                                    if Addr.equal addr dest1 then
                                      match cond with Bil.Var _ when op = Bil.EQ -> Some dest1 | Bil.UnOp (Bil.NOT, Bil.Var _) when op = Bil.NEQ -> Some dest1 | _ -> Some dest2
                                    else if Addr.equal addr dest2 then
                                      match cond with Bil.Var _ when op = Bil.EQ -> Some dest2 | Bil.UnOp (Bil.NOT, Bil.Var _) when op = Bil.NEQ -> Some dest2 | _ -> Some dest1
                                    else
                                      None
                                | _ -> None
                          else
                            None
                       | _ -> None
                     end
                 | _ -> None
             else
               None
         | _ -> None

end
