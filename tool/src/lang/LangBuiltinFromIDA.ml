open Core_kernel.Std
module E = Elf
open Bap.Std

open LangBuiltinBase
open LangUtil.Option

module FN = IDALink.Function
module B = IDALink.Block

type state = {
  strings : String.Set.t;
  imports : Addr.Set.t String.Map.t;
}

module Symtab = struct

  type t = {
    syms_by_name : FN.func String.Map.t;
    syms_by_addr : FN.func Addr.Map.t;
  }

  let empty = {
    syms_by_name = String.Map.empty;
    syms_by_addr = Addr.Map.empty;
  }

  let add t fn = {
    syms_by_name = Map.add t.syms_by_name (FN.name fn) fn;
    syms_by_addr = Map.add t.syms_by_addr (FN.entry fn) fn;
  }

end

module Loader = struct

  let load_file ?path filename =
    let open Or_error in
    let (eh, shs, plt_bound, sym_strs) = In_channel.with_file filename ~binary:true ~f:(fun file ->
      let eh = E.Header.read_from_exn ~file in
      let shs = E.SectionHeader.read_from_exn ~file eh in
      
      let plt_bound = Option.value ~default:(fun _ -> false) @@
        E.Utilities.with_section_with_name eh shs ~file ~name:".plt" ~f:(fun sh ->
          let saddr = sh.E.SectionHeader.sh_addr in
          let eaddr = Int64.(saddr + sh.E.SectionHeader.sh_size) in

          Some (fun addr ->
            let addr' = Or_error.ok_exn @@ Addr.to_int64 addr in
            Int64.(saddr <= addr' && addr' < eaddr)))
      in

      let sym_strs = Option.value_exn (
        E.Utilities.with_section_with_name eh shs ~file ~name:".shstrtab" ~f:(fun sh ->
          E.StringTable.extract eh sh ~file))
      in
      (eh, shs, plt_bound, sym_strs)
    ) in
    Image.create filename >>| fun (img, _) ->
    match IDALink.load ?path img filename with
      | None -> (eh, shs, plt_bound, sym_strs, img, Symtab.empty)
      | Some fns -> (eh, shs, plt_bound, sym_strs, img, Seq.fold fns ~init:Symtab.empty ~f:Symtab.add)

end

let load_syms ~ida_path file =
  let open Option.Monad_infix in
  IDALink.load_syms ~path:ida_path file >>| Seq.fold ~init:String.Set.empty ~f:String.Set.add

let load ~ida_path file =
  try
    Some (In_channel.with_file file ~binary:true ~f:(fun file' ->
      let open Or_error in
      try
        let init = (String.Map.empty, String.Set.empty) in
        let (imports, strings) = match Loader.load_file ~path:ida_path (* "/home/slt/bin/ida-6.8/idal" *) file with
          | Ok (_, shs, plt_bound, sym_strs, img, syms) ->
              let sym_addrs = syms.Symtab.syms_by_addr in
              Map.fold sym_addrs ~init ~f:(fun ~key ~data (imports, strings) ->
                let imports' =
                  if plt_bound key then (
                    let name = FN.name data in
                    (* FIXME *)
                    Map.add imports ~key:name ~data:Addr.Set.empty
                  ) else
                    imports
                in
                let strings' =
                  (* What about the case a string exists more than once in a given binary? *)
                  List.fold (FN.strings data) ~init:strings ~f:Set.add
                in
                (imports', strings'))
          | _ -> init
        in
        { strings; imports }
      with
        | _ -> { strings = String.Set.empty; imports = String.Map.empty }))
  with
    | _ -> None

let import_exists st ~name =
  String.Map.mem st.imports name
