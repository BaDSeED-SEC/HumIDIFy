open Core_kernel.Std
module E = Elf
module S = Symtab
open Bap.Std

module FN = IDALink.Function
module B = IDALink.Block

module FNGraph (Edge : Opaque) = struct

  module E = Edge
  module N = IDALink.Block

  module G = Graphlib.Make (N) (E)
  
  let data_flow ?(dir = `Forward) ~init ~trans ~join g =
    let nodes = G.nodes g in
    let ins_outs = Seq.fold nodes ~init:(G.Node.Map.empty, G.Node.Map.empty) ~f:(fun (transs, joins) node ->
      let (transs_data, joins_data) = init node in
      (G.Node.Map.add transs ~key:node ~data:transs_data, G.Node.Map.add joins ~key:node ~data:joins_data))
    in

    let wq = Queue.create () in
    Seq.iter ~f:(Queue.enqueue wq) nodes;

    let rec aux ((transs, joins) as acc) = match Queue.dequeue wq with
      | None -> acc
      | Some n ->
          let (nf, uf) = match dir with
            | `Forward -> G.Node.succs, G.Node.preds
            | `Backward -> G.Node.preds, G.Node.succs
          in

          let transs_p = G.Node.Map.find_exn transs n in
          let joins_p = G.Node.Map.find_exn joins n in

          let transs_n = trans g n transs_p joins_p in

          let njs = Seq.map (nf n g) ~f:(fun n' ->
            let t' = G.Node.Map.find_exn transs n' in
            let j' = G.Node.Map.find_exn joins n' in
            (n', t', j'))
          in

          let joins_n = join g n transs_p joins_p njs in

          if transs_n = None && joins_n = None then
            aux acc
          else (
            let transs_n' = Option.value ~default:transs_p transs_n in
            let joins_n' = Option.value ~default:joins_p joins_n in

            let transs' = G.Node.Map.add transs ~key:n ~data:transs_n' in
            let joins' = G.Node.Map.add joins ~key:n ~data:joins_n' in

            Seq.iter (uf n g) ~f:(fun n -> if not (Queue.mem wq n) then Queue.enqueue wq n);

            aux (transs', joins')
          )
    in
    aux ins_outs

  let of_fn ~label fn =
    let blocks = FN.blocks fn in
    let (nodes, g) = Addr.Table.fold blocks ~init:(B.Map.empty, G.empty) ~f:(fun ~key:_ ~data:block (nodes, g') ->
      let n = G.Node.create block in
      (Map.add nodes block n, G.Node.insert n g'))
    in
    B.Map.fold nodes ~init:g ~f:(fun ~key:block ~data:src g' ->
      List.fold (B.local_dests block) ~init:g' ~f:(fun g'' -> function
        | `Cond (_, dest) | `Fall (_, dest) ->
            let e = G.Edge.create src (Map.find_exn nodes dest) (label block dest) in
            G.Edge.insert e g''
        | `Call _ -> g''))

end

module G = BlockGraph

module FG = FNGraph (G.Edge)
module DA = DataAnalysis.Make (ARM)

let is_called ~image ~syms name = match Map.find syms.S.syms_by_name name with
  | None -> false
  | Some callee ->
      let addr = FN.entry callee in
      Map.exists syms.S.syms_by_addr ~f:(fun fn ->
        let bound = FN.bound fn in
        Addr.Table.exists (FN.blocks fn) ~f:(fun block ->
          List.exists (B.calls ~image ~bound block) ~f:(fun callee ->
            Addr.equal addr callee)))

let calls_to ~image ~syms ~arity name = match Map.find syms.S.syms_by_name name with
  | None -> []
  | Some _ when arity > 10 || arity < 0 -> []
  | Some callee ->
      let ftab =
        let open ARM.CPU in
        `Heuristic (List.take [r0; r1; r2; r3; r4; r5; r6; r7; r8; r9; r10]  arity)
      in
      let addr = FN.entry callee in

      Map.fold syms.S.syms_by_addr ~init:[] ~f:(fun ~key:_ ~data:fn acc ->
        let bound = FN.bound fn in
        Addr.Table.fold (FN.blocks fn) ~init:acc ~f:(fun ~key ~data:block acc' ->
          match B.dependent_call ~image ~bound ~ftab block with
            | None -> acc'
            | Some (addr', m) ->
                (* Do best effort: if there aren't enough arguments, then skip *)
                if not (Addr.equal addr addr') || Var.Map.length m <> arity then
                  acc'
                else
                  m :: acc'))
