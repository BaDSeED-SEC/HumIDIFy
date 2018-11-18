open Core_kernel.Std
module E = Elf
module S = Symtab
open Bap.Std
open FunGraph

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
      | None -> (eh, shs, plt_bound, sym_strs, img, S.empty)
      | Some fns -> (eh, shs, plt_bound, sym_strs, img, Seq.fold fns ~init:S.empty ~f:S.add)

end

let find_ida () =
  let oc = Unix.open_process_in "which idal" in
  match In_channel.input_line oc with
    | None -> print_endline "[e] unable to find IDA Pro."; In_channel.close oc; exit 0
    | Some path -> In_channel.close oc; path

let ida_path = find_ida ()

let classify ~prefix file =
  let in_sexp = prefix ^ ".sexp" in
  let (cls, imps, strs) = In_channel.with_file in_sexp ~f:(fun f ->
    let cls = List.t_of_sexp String.t_of_sexp (Sexp.input_sexp f) in
    let imps = String.Set.t_of_sexp (Sexp.input_sexp f) in
    let strs = String.Set.t_of_sexp (Sexp.input_sexp f) in
    (cls, imps, strs))
  in
  let header_arff = In_channel.read_all (prefix ^ ".arff") in
  let input_model = prefix ^ ".model" in

  let cls_arff = Filename.temp_file "cls" ".arff" in

  let (shs, plt_bound, sym_strs, img, syms, imps, strs) =
    Out_channel.with_file cls_arff ~f:(fun f ->
      output_string f header_arff;
      let open Or_error in
      try
        match Loader.load_file ~path:ida_path file with
          | Ok (_, shs, plt_bound, sym_strs, img, syms) ->
              let sym_addrs = syms.S.syms_by_addr in
              let (imps', strs') =
                Map.fold sym_addrs ~init:(String.Set.empty, String.Set.empty) ~f:(fun ~key ~data (imps', strs') ->
                  let imps' =
                    if plt_bound key then
                      Set.add imps' (FN.name data)
                    else
                      imps'
                  in
                  let strs' = List.fold (FN.strings data) ~init:strs' ~f:String.Set.add in
                  (imps', strs'))
              in

              let print_string = output_string f in
              let line = Buffer.create 10 in

              Set.iter imps ~f:(fun imp ->
                if Set.mem imps' imp then
                  Buffer.add_string line "1,"
                else
                  Buffer.add_string line "0,");

              Set.iter strs ~f:(fun str ->
                if Set.mem strs' str then
                  Buffer.add_string line "1,"
                else
                  Buffer.add_string line "0,");

              print_string @@ (Buffer.contents line) ^ "?\n";
              (shs, plt_bound, sym_strs, img, syms, imps', strs')
          | _ ->
              eprintf "error: unable to load features from %s.\n" file;
              exit (-1)
        with
          | _ ->
              eprintf "error: unable to load %s\n" file;
              exit (-1))
  in

  let cls_ic = Unix.open_process_in @@ sprintf "java -cp /usr/share/java/weka/weka.jar weka.classifiers.bayes.BayesNet -T %s -classifications weka.classifiers.evaluation.output.prediction.CSV -p 0 -l %s" cls_arff input_model in

  begin try
    let rec skip () =
      if input_line cls_ic <> "inst#,actual,predicted,error,prediction" then
        skip ()
    in skip ()
  with
    | exn ->
        ignore @@ Unix.close_process_in cls_ic;
        raise exn
  end;

  let split = Str.split_delim (Str.regexp ",") in

  match try Some (input_line cls_ic |> split) with _ -> None with
    | Some [_; _; pred; _; prediction] ->
        let label = String.drop_prefix pred (String.index_exn pred ':' + 1) in
        Unix.close_process_in cls_ic |> ignore;
        ((label, Float.of_string prediction), shs, plt_bound, sym_strs, img, syms, imps, strs)
    | _ ->
        Unix.close_process_in cls_ic |> ignore;
        eprintf "error: failed to classify %s\n" file;
        exit (-1)

let () =
  try
    print_endline "]] FunFile: version 1.0 ,-.";
    print_endline "]-----------------------|-'";
    if Array.length Sys.argv = 3 then
      let prefix = Sys.argv.(1) in
      let binary = Sys.argv.(2) in

      let ((label, confidence), shs, plt_bound, sym_strs, img, syms, imps, strs) =
        classify ~prefix binary
      in

      printf "-> File      : %s\n-> Type      : %s (with confidence %.2f%%)\n\n" binary label (confidence *. 100.);
    else
      eprintf "usage: %s <classifier-prefix> <binary>\n" Sys.argv.(0);
      exit (-1)
  with
    | Invalid_argument s -> printf "[e] %s\n" s
