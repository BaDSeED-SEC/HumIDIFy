open Core_kernel.Std
open Bap.Std

module J48 = struct
  (* WEKA classification *)
  let j48 ?class_path
          ?test_file
          ?model
          ()
          =
    let open WekaSystem in
    args begin
      (* Class path *)
      opt_string "-cp"
      (* Classifier name *)
      & anon_string
      (* Test file *)
      & opt_string "-T"
      (* Predictions *)
      & opt_string "-classifications"
      & opt_int "-p"
      (* Model file *)
      & opt_string "-l"
    end
      class_path
      "weka.classifiers.trees.J48"
      test_file
      (Some "weka.classifiers.evaluation.output.prediction.CSV")
      (Some 0)
      model
    |> run "java"

  let classify ~class_path ~test_file ~model =
    let (ic, _, _) as p = j48 ~class_path ~test_file ~model () in
    begin try
      let rec skip () =
        if input_line ic <> "inst#,actual,predicted,error,prediction" then
          skip ()
      in skip ()
    with
      | exn ->
          ignore @@ Unix.close_process_full p;
          raise exn
    end;

    let split = Str.split_delim (Str.regexp ",") in

    let rec collect acc =
      match try Some (input_line ic |> split) with _ -> None with
        | Some [inst; _; pred; _; prob] ->
            collect ((int_of_string inst, String.drop_prefix pred (String.index_exn pred ':' + 1), Float.of_string prob) :: acc)
        | _ -> List.rev acc
    in
    let res = collect [] in
    ignore @@ Unix.close_process_full p;
    res

  let j48_learn ?class_path
                ?test_file
                ?model
                ()
                =
    let open WekaSystem in
    args begin
      (* Class path *)
      opt_string "-cp"
      (* Classifier name *)
      & anon_string
      (* Test file *)
      & opt_string "-T"
      (* Model file *)
      & opt_string "-d"
    end
      class_path
      "weka.classifiers.trees.J48"
      test_file
      model
    |> run "java"

  let learn ~class_path ~test_file ~model =
    let (ic, _, _) as p = j48_learn ~class_path ~test_file ~model () in
    ignore @@ In_channel.input_all ic;
    ignore @@ Unix.close_process_full p
end

module RandomForest = struct
  (* WEKA classification *)
  let random_forest ?class_path
                    ?test_file
                    ?model
                    ()
                    =
    let open WekaSystem in
    args begin
      (* Class path *)
      opt_string "-cp"
      (* Classifier name *)
      & anon_string
      (* Test file *)
      & opt_string "-T"
      (* Predictions *)
      & opt_string "-classifications"
      & opt_int "-p"
      (* Model file *)
      & opt_string "-l"
      (* Unclassified instances *)
      & anon_string
      (* Number of parallel execution slots *)
      & opt_int "-num-slots"
    end
      class_path
      "weka.classifiers.trees.RandomForest"
      test_file
      (Some "weka.classifiers.evaluation.output.prediction.CSV")
      (Some 0)
      model
      "-U"
      (Some 0)
    |> run "java"

  let classify ~class_path ~test_file ~model =
    let (ic, _, _) as p = random_forest ~class_path ~test_file ~model () in
    begin try
      let rec skip () =
        if input_line ic <> "inst#,actual,predicted,error,prediction" then
          skip ()
      in skip ()
    with
      | exn ->
          ignore @@ Unix.close_process_full p;
          raise exn
    end;

    let split = Str.split_delim (Str.regexp ",") in

    let rec collect acc =
      match try Some (input_line ic |> split) with _ -> None with
        | Some [inst; _; pred; _; prob] ->
            collect ((int_of_string inst, String.drop_prefix pred (String.index_exn pred ':' + 1), Float.of_string prob) :: acc)
        | _ -> List.rev acc
    in
    let res = collect [] in
    ignore @@ Unix.close_process_full p;
    res

  let random_forest_learn ?class_path
                          ?test_file
                          ?model
                          ()
                          =
    let open WekaSystem in
    args begin
      (* Class path *)
      opt_string "-cp"
      (* Classifier name *)
      & anon_string
      (* Test file *)
      & opt_string "-T"
      (* Model file *)
      & opt_string "-d"
      (* Unclassified instances *)
      & anon_string
      (* Number of parallel execution slots *)
      & opt_int "-num-slots"
    end
      class_path
      "weka.classifiers.trees.RandomForest"
      test_file
      model
      "-U"
      (Some 0)
    |> run "java"

  let learn ~class_path ~test_file ~model =
    let (ic, _, _) as p = random_forest_learn ~class_path ~test_file ~model () in
    ignore @@ In_channel.input_all ic;
    ignore @@ Unix.close_process_full p
end

let infer_class_path () =
  let ((ic, oc) as p) = Unix.open_process "which weka" in
  let path = In_channel.input_all ic |> String.rstrip in
  Unix.close_process p;
  path

