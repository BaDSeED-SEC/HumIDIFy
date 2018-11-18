open Core_kernel.Std
open Bap.Std

module Node = struct
 
  module Arg = struct

    type t = [ `Data of int | `Other ]

    let create ?refs () = match refs with
      | Some n when n > 0 -> `Data n
      | _ -> `Other

    let is_data = (=) `Other
    let n_refs = function
      | `Other -> None
      | `Data n -> Some n

  end

  type node = {
    id      : int;
    addr    : addr;
    insns   : int;
    fn_call : (string * Arg.t list) option;
  }

  include Opaque.Make (struct
    type t = node
  
    let hash { id } = Int.hash id
    let compare n1 n2 = Int.compare n1.id n2.id
  end)

  type t = node

  let id_gen =
    let id = ref 0 in
    fun () -> let n = !id in id := Int.succ !id; n

  let create ?fn_call addr insns = {
    id      = id_gen ();
    addr    = addr;
    insns   = insns;
    fn_call = fn_call;
  }

  let id { id } = id
  let addr { addr } = addr
  let insns { insns } = insns
  let args { fn_call } = Option.(>>|) fn_call snd

end

module Edge = struct

  type edge = unit

  include Opaque.Make (struct
    type t = edge

    let hash = Unit.hash
    let compare = Unit.compare
  end)

  type t = edge

  let create () = ()
  
end

module Graph = struct

  module N = Node
  module E = Edge
  
  module G = Graphlib.Make (Node) (Edge)

  (** Computes $reachable(n)$ *)
  let compute_reachable g =
    let v = Seq.fold (G.nodes g) ~init:G.Node.Map.empty ~f:(fun m n ->
      let r = Graphlib.fold_reachable (module G)
                                      ~init:G.Node.Set.empty
                                      ~f:Set.add
                                      g
                                      n
      in
      Map.add ~key:n ~data:(Set.remove r n) m)
    in
    v

  let reachable m n =
    Set.to_list (Map.find_exn m n)

  let is_decision g n =
    G.Node.degree ~dir:`Out n g > 1

  let is_switch g n =
    G.Node.degree ~dir:`Out n g > 2

  (** Computes $D(G)$ *)
  let decision g =
    List.filter ~f:(is_decision g)

  (** Computes $D_{switch}(G)$ *)
  let switch g =
    List.filter ~f:(is_switch g)

  (*
    Si = R(Bi)

    for each Bj in D(Bi):
            Si = Si / R(Bj)

    for each Bj in ND(Si):
            Si = Si U R(Bj)

    Perform symmetric diff of sets in D(Bi)
  *)

  (** Computes $guard(n)$ *)
  (* NOTE: Computes the unique nodes that are reachable by decision points *)
  let compute_unique g m =
    let m1 = Map.mapi m ~f:(fun ~key ~data ->
      Set.fold data
               ~init:data
               ~f:(fun s' n' ->
                    if is_decision g n' then
                      Set.diff s' (Map.find_exn m n')
                    else
                      s'))
    in

    let m2 = Map.mapi m1 ~f:(fun ~key ~data ->
      Set.fold data
               ~init:data
               ~f:(fun s' n' ->
                    if not @@ is_decision g n' then
                      Set.union s' (Map.find_exn m n')
                    else
                      s'))
    in

    let m3 = Map.fold m2 ~init:G.Node.Map.empty ~f:(fun ~key ~data m2' ->
      if is_decision g key then
        let data = Option.value (Map.find m2' key) ~default:data in
        let (m2'', s) = Set.fold data
                         ~init:(m2', data)
                         ~f:(fun ((m2'', s) as v) n ->
                              if is_decision g n then
                                let s_n = Map.find_exn m2 n in
                                let s' = Set.diff s s_n in
                                let s_n' = Set.diff s_n s in
                                (Map.add ~key:n ~data:s_n' m2'', s')
                              else
                                v)
        in
        Map.add ~key ~data:s m2''
      else
        m2')
    in
    m3

  let unique m n =
    Set.to_list (Map.find_exn m n)

  (*
  let compute_branch_paths g u =
    let m = G.Node.Map.fold u ~init:G.Node.Map.empty ~f:(fun ~key ~data p ->
      if is_decision g key then
        G.Node.Map.add p ~key ~data:(List.filter data ~f:(is_decision g))
      else
        p)
    in

    let rec d v seen =
      if not @@ G.Node.Set.mem seen v then
        let data = G.Node.Map.find_exn m v in
        Some (List.map ~f:((::) v) @@ List.map data ~f:(fun v' -> d v' (G.Node.Set.remove seen v)))
      else
        None
    in
  *)

  (* NOTE: Do we need to perform a topological sort? *)
  let compute_branch_paths g u =
    let q = ref @@ Set.of_map_keys (G.Node.Map.filter ~f:(fun ~key ~data -> is_decision g key) u) in
    let p = G.Node.Table.create () in
    let rec d n seen =
      q := Set.remove !q n;
      if not @@ Set.mem seen n then (
        let p_n = List.fold (unique u n) ~init:[] ~f:(fun p_n n' ->
          if is_decision g n' then
            match d n' (Set.remove seen n) with
              | None -> p_n
              | Some v ->
                  List.fold v ~init:p_n ~f:(fun p_n' n's -> (n :: n's) :: p_n')
           else
             p_n)
        in
        if p_n = [] then
          Some [[n]]
        else
          Some p_n
      ) else
        None
    in
    let rec aux () = match Set.choose !q with
      | None -> p
      | Some n ->
          match d n G.Node.Set.empty with
            | None -> aux ()
            | Some p_n -> ignore (G.Node.Table.add p n p_n); aux ()
    in
    aux ()

  let branch_paths = G.Node.Table.find

  let max_indicent_edges g =
    Seq.fold (G.nodes g) ~init:0 ~f:(fun acc n ->
      acc + G.Node.degree ~dir:`In n g)

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
(*
      | Some n when dir = `Forward -> begin
          let joins_n = trans g n (G.Node.Map.find_exn joins n) in
          let transs_n = Seq.fold (G.Node.preds n g) ~init:(G.Node.Map.find_exn transs n) ~f:(join g n) in

          let transs' = G.Node.Map.add transs ~key:n ~data:transs_n in

          match joins_n with
            | None -> aux (transs', joins)
            | Some joins_n' ->
                let joins' = G.Node.Map.add joins ~key:n ~data:joins_n' in
                Seq.iter (G.Node.succs n g) ~f:(fun n -> if not (Queue.mem wq n) then Queue.enqueue wq n);
                aux (transs', joins')
end
*)
      | Some n ->
          let (nf, uf) = match dir with
            | `Forward -> G.Node.succs, G.Node.preds
            | `Backward -> G.Node.preds, G.Node.succs
          in

          let transs_p = G.Node.Map.find_exn transs n in
          let joins_p = G.Node.Map.find_exn joins n in

          let transs_n = trans g n transs_p joins_p in

          let njs = Seq.map (uf n g) ~f:(fun n' ->
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

            Seq.iter (nf n g) ~f:(fun n' ->  Queue.enqueue wq n);

            aux (transs', joins')
          )
(* 
      | Some n when dir = `Backward -> begin
          let transs_n = trans g n (G.Node.Map.find_exn joins n) (G.Node.Map.find_exn transs n) in
          let njs = Seq.map (G.Node.preds n g) ~f:(fun n' ->
            let t' = G.Node.Map.find_exn transs n' in
            let j' = G.Node.Map.find_exn joins n' in
            (n, t', j'))
          in
          let joins_n = join g n (G.Node.Map.find_exn joins n) (G.Node.Map.find_exn transs n) njs in

          let joins' = G.Node.Map.add joins ~key:n ~data:joins_n in

          match transs_n with
            | None -> aux (transs, joins')
            | Some transs_n' ->
                let transs' = G.Node.Map.add transs ~key:n ~data:transs_n' in
                Seq.iter (G.Node.succs n g) ~f:(fun n -> if not (Queue.mem wq n) then Queue.enqueue wq n);
                aux (transs', joins')
        end

      | _ -> assert false
*)

      (* TODO: We can generalise this and the above case 
      | Some n when dir = `Backward ->
          let in_n = trans g n (G.Node.Map.find_exn ins n) in
          let out_n = Seq.fold (G.Node.succs n g) ~init:(G.Node.Map.find_exn outs n) ~f:(join g n) in

          let outs' = G.Node.Map.add outs ~key:n ~data:out_n in

          match in_n with
            | None -> aux (ins, outs')
            | Some in_n' ->
                let ins' = G.Node.Map.add ins ~key:n ~data:in_n' in
                Seq.iter (G.Node.preds n g) ~f:(fun n -> if not (Queue.mem wq n) then Queue.enqueue wq n);
                aux (ins', outs')
      *)
    in
    aux ins_outs

  let of_fn fn =
    let module FN = IDALink.Function in
    let module BL = IDALink.Block in

    let blocks = FN.blocks fn in
    let (nodes, g) = Addr.Table.fold blocks ~init:(BL.Map.empty, G.empty) ~f:(fun ~key:addr ~data:block (nodes, g') ->
      let n = G.Node.create (Node.create addr @@ List.length @@ BL.insns block) in
      (Map.add nodes block n, G.Node.insert n g'))
    in
    BL.Map.fold nodes ~init:g ~f:(fun ~key:block ~data:src g' ->
      List.fold (BL.local_dests block) ~init:g' ~f:(fun g'' -> function
        | `Cond (_, dest) | `Fall (_, dest) ->
            let e = G.Edge.create src (Map.find_exn nodes dest) () in
            G.Edge.insert e g''
        | `Call _ -> g''))

  include G

end
