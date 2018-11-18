open Core_kernel.Std
module E = Elf
open Bap.Std

module FN = IDALink.Function
module B = IDALink.Block

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

module ARFF = struct

  let generate ~symbols ~strings ~classes output =
    let generate_label tag n s = sprintf "@ATTRIBUTE %s_%d {0, 1} %% %s\n" tag n (String.escaped s) in
    let buf = Buffer.create 100 in

    Buffer.add_string buf "@RELATION functionality\n\n";

    Buffer.add_string buf "% Symbols\n";
    let _ = Set.fold symbols ~init:0 ~f:(fun n str -> Buffer.add_string buf (generate_label "api" n str); n + 1) in

    Buffer.add_string buf "\n% Strings\n";
    let _ = Set.fold strings ~init:0 ~f:(fun n str -> Buffer.add_string buf (generate_label "str" n str); n + 1) in

    Buffer.add_string buf "\n% Classes\n@ATTRIBUTE class {";
    List.intersperse classes ~sep:", " |> List.iter ~f:(Buffer.add_string buf);
    Buffer.add_string buf "}\n\n@DATA\n";

    Buffer.output_buffer output buf

end

let inclusion_delta = 0.0

let () =
  if Array.length Sys.argv >= 3 then
    let base_dir = Sys.argv.(1) in
    let output_prefix = Sys.argv.(2) in

    let (cls, imps, strs) = List.fold_left (FileUtil.ls base_dir) ~init:([], String.Set.empty, String.Set.empty) ~f:(fun ((cls, imps, strs) as acc) name ->
      if FileUtil.test FileUtil.Is_dir name && name <> "unknown" then
        let label = FilePath.basename name in
        let files = FileUtil.ls name in

        let num_candidates = List.length files in
        let min_inclusion = Int.of_float @@ Float.round_up (Float.of_int num_candidates *. inclusion_delta) in

        printf "[!] processing %d files for label \"%s\" (min. incl.: %d):\n" num_candidates label min_inclusion;

        let import_counts = String.Map.empty in
        let string_counts = String.Map.empty in

        let (import_counts', string_counts') =
          List.fold_left files ~init:(import_counts, string_counts) ~f:(fun acc' file ->
            printf "[!] processing file \"%s\":\n" file;
            flush_all ();
            In_channel.with_file file ~binary:true ~f:(fun file' ->
              let open Or_error in
              try
                match Loader.load_file ~path:"/home/slt/bin/ida-6.8/idal" file with
                  | Ok (_, shs, plt_bound, sym_strs, img, syms) ->
                      let sym_addrs = syms.Symtab.syms_by_addr in
                      Map.fold sym_addrs ~init:acc' ~f:(fun ~key ~data (import_counts', string_counts') ->
                        let import_counts'' =
                          if plt_bound key then
                            let name = FN.name data in
                            match Map.find import_counts' name with
                              | None -> Map.add import_counts' ~key:name ~data:1
                              | Some count -> Map.add import_counts' ~key:name ~data:(succ count)
                          else
                            import_counts'
                        in
                        let string_counts'' =
                          (* What about the case a string exists more than once in a given binary? *)
                          List.fold (FN.strings data) ~init:string_counts' ~f:(fun counts str ->
                            match Map.find counts str with
                              | None -> Map.add counts ~key:str ~data:1
                              | Some count -> Map.add counts ~key:str ~data:(succ count))
                        in
                        (import_counts'', string_counts''))
                  | _ -> acc'
              with
                | _ -> acc'))
        in

        let cls' = label :: cls in
        let imps' = Map.fold import_counts' ~init:imps ~f:(fun ~key ~data imps' ->
          if data >= min_inclusion then
            Set.add imps' key
           else
             imps')
        in
        let strs' = Map.fold string_counts' ~init:strs ~f:(fun ~key ~data strs' ->
          if data >= min_inclusion then
            Set.add strs' key
           else
             strs')
        in
        (cls', imps', strs')
      else
        acc
    )
    in

    let out_arff = output_prefix ^ ".arff" in
    Out_channel.with_file out_arff ~f:(ARFF.generate ~symbols:imps ~strings:strs ~classes:cls);
    printf "[!] output ARFF to: %s\n" out_arff;

    let out_bin = output_prefix ^ ".sexp" in
    Out_channel.with_file out_bin ~f:(fun f ->
      Sexp.output_mach f (List.sexp_of_t String.sexp_of_t cls);
      Sexp.output_mach f (String.Set.sexp_of_t imps);
      Sexp.output_mach f (String.Set.sexp_of_t strs));
    printf "[!] output SEXP to: %s\n" out_bin;

    print_endline "[!] process complete."
  else
    print_endline "[e] cannot read from data set directory."
