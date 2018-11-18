open Core_kernel.Std
module E = Elf
open Bap.Std

open LangAst
open LangUtil.Option
open LangBuiltinBase

module Environment = LangTypeChecker.Environment

type ctx = {
  channel     : in_channel;
  shs         : E.SectionHeader.t array;
  sym_strs    : E.StringTable.t;
  elf_prop    : LangBuiltinElf.state;
  str_prop    : LangBuiltinStrings.state;
  str_refs    : String.Set.t;
  fn_called   : string -> bool;
  fn_calls_to : arity:int -> string -> (mem * Bil.exp) Var.Map.t list;
  syms        : String.Set.t;
}

let function_args ctx name args =
  let arity = Array.length args in
  let reg2var =
    let open ARM.CPU in
    let regs = [|r0; r1; r2; r3; r4; r5; r6; r7; r8; r9; r10|] in
    Array.filter_mapi args ~f:(fun i -> function None -> None | Some v -> Some (regs.(i), v))
  in
  List.fold (ctx.fn_calls_to ~arity name) ~init:[] ~f:(fun acc m ->
    let v = Array.fold reg2var ~init:[] ~f:(fun acc' (reg, (var, ty)) ->
      match Var.Map.find m reg with
        | None -> acc'
        | Some (_, exp) ->
            match ty with
              | TBOOL -> 
                  begin match exp with
                    | Bil.Int v -> (var, BOOL ((Word.to_int v |> Or_error.ok_exn) <> 0)) :: acc'
                    | _ -> acc'
                  end
              | TINT -> 
                  begin match exp with
                    | Bil.Int v -> (var, INT (Word.to_int64 v |> Or_error.ok_exn)) :: acc'
                    | _ -> acc'
                  end
              | TSTRING ->
                  begin match FunGraph.DA.of_exp ctx.sym_strs ctx.shs exp with
                    | Some { DataAnalysis.loc_offset = Some at } ->
                        begin match DataAnalysis.Extraction.extract ~with_type:(`S_String `ASCII_Z) ~from:(`File ctx.channel) ~at with
                          | Some (`String str) -> (var, STRING str) :: acc'
                          | _ -> acc'
                        end
                    | _ -> acc'
                  end
              | _ -> acc')
    in v :: acc)

(* Dispatcher for builtins *)
let builtin ctx name args = match name, args with
  | "import_exists", [| STRING name |] -> BOOL (String.Set.mem ctx.syms name)
  | "import_exists", [| BOTTOM |] -> BOTTOM
  | "export_exists", [| STRING name |] -> LangBuiltinElf.exports ctx.elf_prop ~name
  | "export_exists", [| BOTTOM |] -> BOTTOM
  | "string_exists", [| STRING name |] -> LangBuiltinStrings.contains ctx.str_prop ~name
  | "string_exists", [| BOTTOM |] -> BOTTOM
  | "function_ref", [| STRING name |] -> BOOL (String.Set.mem ctx.syms name)
  | "function_ref", [| BOTTOM |] -> BOTTOM
  | "string_ref", [| STRING name |] -> BOOL (String.Set.mem ctx.str_refs name)
  | "string_ref", [| BOTTOM |] -> BOTTOM
  | "architecture", [| STRING name |] -> LangBuiltinElf.architecture ctx.elf_prop ~name
  | "architecture", [| BOTTOM |] -> BOTTOM
  | "endianness", [| STRING name |] -> LangBuiltinElf.endianness ctx.elf_prop ~name
  | "endianness", [| BOTTOM |] -> BOTTOM
  | "print", [| STRING str |] -> printf "-> %s\n" str; BOOL true
  | _ -> invalid_arg "builtin undefined"

(* Register builtin rules into the environment *)
let register_builtins env =
  let builtins = [
   "import_exists", TBOOL, [| "", TSTRING |], BUILTIN;
   "export_exists", TBOOL, [| "", TSTRING |], BUILTIN;
   "string_exists", TBOOL, [| "", TSTRING |], BUILTIN;
   "function_ref" , TBOOL, [| "", TSTRING |], BUILTIN;
   "string_ref"   , TBOOL, [| "", TSTRING |], BUILTIN;
   "architecture" , TBOOL, [| "", TSTRING |], BUILTIN;
   "endianness"   , TBOOL, [| "", TSTRING |], BUILTIN;
   "print"        , TBOOL, [| "", TSTRING |], BUILTIN;
  ] in
  List.fold_left builtins ~init:env ~f:(fun env' (name, ty, tys, e) ->
    Environment.add_rule name (ty, tys) e env')

let initialise ~ida_path (shs, bound, sym_strs, image, syms, imps, strs) file =
  let ic = open_in_bin file in
  LangBuiltinElf.load ic >>= fun elf_prop ->
  LangBuiltinStrings.load ic >>= fun str_prop ->
  LangBuiltinFromIDA.load_syms ~ida_path file >>| fun syms' -> {
    channel  = ic;
    shs = shs;
    sym_strs = sym_strs;
    elf_prop = elf_prop;
    str_prop = str_prop;
    str_refs = strs;
    fn_called = FunGraph.is_called ~image ~syms;
    fn_calls_to = FunGraph.calls_to ~image ~syms;
    syms = syms';
  }

let clean_up { channel } =
  close_in_noerr channel
