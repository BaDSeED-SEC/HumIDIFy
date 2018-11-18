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

type result = {
  model        : string;
  iterations   : int;
  unclassified : int
}

let classify ~delta ~classifier ~arff instances =
  List.fold (classifier arff) ~init:false ~f:(fun acc (inst, cls, pred) ->
    if pred >= delta then (
      let path = Array.get instances inst in
      let new_path = Filename.dirname (Filename.dirname path) ^ "/" ^ cls ^ "/" ^ Filename.basename path in
      FileUtil.mv path new_path;
      true
    ) else
      acc)

let learn ?(delta = 0.8) ~learner ~classifier base_dir =
  let rec fix_point n =
    let (model, instances, arff) = learner base_dir in
    let count = Array.length instances in

    if classify ~delta ~classifier ~arff instances then
      fix_point (succ n)
    else
      { model; iterations = n; unclassified = count }
  in
  fix_point 0

let gen_arff ~hdr ~imps ~strs ~cls base_dir =
  let (training, trn_arff) = Filename.open_temp_file "trn" ".arff" in
  let (testing, tst_arff) = Filename.open_temp_file "tst" ".arff" in

  Out_channel.output_string trn_arff hdr;
  Out_channel.output_string tst_arff hdr;

  let instances = List.fold (FileUtil.ls base_dir) ~init:[] ~f:(fun instances dir ->
    let label = FilePath.basename dir in
    let files = FileUtil.ls dir in

    List.fold files ~init:instances ~f:(fun instances' file ->
      In_channel.with_file file ~binary:true ~f:(fun inf ->
        let open Or_error in
        match Loader.load_file ~path:"/home/slt/bin/ida-6.8/idal" file with
          | Ok (_, shs, plt_bound, sym_strs, img, syms) ->
              let sym_addrs = syms.Symtab.syms_by_addr in
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
              
              let (instances', rcls, f) =
                if List.mem ~equal:String.equal cls label then
                  (instances', label, trn_arff)
                else 
                  (file :: instances', "?", tst_arff)
              in

              let print_string = Out_channel.output_string f in

              Set.iter imps ~f:(fun imp ->
                if Set.mem imps' imp then
                  print_string "1,"
                else
                  print_string "0,");

              Set.iter strs ~f:(fun str ->
                if Set.mem strs' str then
                  print_string "1,"
                else
                  print_string "0,");

              print_string @@ rcls ^ "\n";
              instances'
        | _ -> instances')))
  in

  Out_channel.close tst_arff;
  Out_channel.close trn_arff;

  (training, testing, Array.of_list_rev instances)


let create_learner ~alg ~gen_arff base_dir =
  let model = Filename.temp_file "mdl" ".model" in

  let (training, testing, instances) = gen_arff base_dir in
  alg ~class_path:(Weka.infer_class_path ()) ~test_file:training ~model;
  (model, instances, testing)
