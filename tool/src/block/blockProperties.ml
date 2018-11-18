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

end

(** TODO:
    - Separate symbol table
    - Identify destinations
*)

(*
let calls_to ~addr ~args ~init block =
  let number_of_args = List.length args in

  
      Seq.fold (Block.dests block) ~init:acc' ~f:(fun acc'' -> function
        | `Block (dest, _) when Addr.compare (Block.addr dest) addr = 0 ->
            (*
               If we are not doing argument detection, then pass the block
               directly to the predicate.
             *)
            if number_of_args = 0 then
              f acc'' g_block block Var.Map.empty
            else
              let stmts = List.concat_map (Block.insns block) ~f:(fun (mem, insn) ->
                List.cartesian_product [mem] (Insn.bil insn))
              in
              let args = List.fold stmts ~init:Var.Map.empty ~f:(fun args' (mem, stmt) ->
                let stmt' = Arm.resolve_pc mem [stmt]
                         |> Arm.fold_constants
                         |> Arm.resolve_indirects memory
                         |> List.hd_exn
                in
                match stmt' with
                  | Bil.Move (var, expr) ->

                      if Map.length args' < number_of_args
                      && List.mem ~equal:Var.equal args var
                      && not @@ Map.mem args' var
                      then
                        Map.add args' ~key:var ~data:(mem, expr)
                      else
                        args'
                    | _ ->
                        args')
              in f acc'' g_block block args
        | _ -> acc'')))
*)
