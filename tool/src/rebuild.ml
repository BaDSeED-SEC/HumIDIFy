open Core_kernel.Std
open Bap.Std

let () =
  if Array.length Sys.argv >= 4 then
    let input_arff = Sys.argv.(1) in
    let input_prefix = Sys.argv.(2) in
    let output_prefix = Sys.argv.(3) in

    let input_sexp = input_prefix ^ ".sexp" in
    let output_sexp = output_prefix ^ ".sexp" in

    let (cls, imps, strs) = In_channel.with_file input_sexp ~f:(fun f ->
      let cls = List.t_of_sexp String.t_of_sexp (Sexp.input_sexp f) in
      let imps = Set.to_array @@ String.Set.t_of_sexp (Sexp.input_sexp f) in
      let strs = Set.to_array @@ String.Set.t_of_sexp (Sexp.input_sexp f) in
      (cls, imps, strs))
    in

    let (imps', strs') =
      In_channel.with_file input_arff ~f:(fun f ->
        In_channel.fold_lines f ~init:(String.Set.empty, String.Set.empty) ~f:(fun (imps', strs') s ->
          match String.split ~on:' ' s with
            | ("@ATTRIBUTE" | "@attribute") :: label :: _ ->
                begin match String.split ~on:'_' label with
                  | "api" :: num :: _ ->
                      let n = int_of_string num in
                      (String.Set.add imps' imps.(n), strs')
                  | "str" :: num :: _ ->
                      let n = int_of_string num in
                      (imps', String.Set.add strs' strs.(n))
                  | _ ->
                      (imps', strs')
                end
            | _ -> (imps', strs')))
    in

    Out_channel.with_file output_sexp ~f:(fun f ->
      Sexp.output_mach f (List.sexp_of_t String.sexp_of_t cls);
      Sexp.output_mach f (String.Set.sexp_of_t imps');
      Sexp.output_mach f (String.Set.sexp_of_t strs'));

    printf "[!] output SEXP to: %s\n" output_sexp;
    print_endline "[!] process complete."
  else
    printf "[u] %s <input-arff> <input-prefix> <output-prefix>\n" Sys.argv.(0)
