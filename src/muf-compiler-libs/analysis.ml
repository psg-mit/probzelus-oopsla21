open Muf
module VarMap = Map.Make (String)
module RVSet = Set.Make (Int)
module RVMap = Map.Make (Int)

let flat_map f s = RVSet.fold (fun v acc -> RVSet.union acc (f v)) s RVSet.empty

module Rep = struct
  (* Lower bound of used RVs, upper bound of used RVs *)
  type scalar = RVSet.t * RVSet.t

  (* The abstract representation of a uF term has the same rep as its type *)
  type rep = Rscalar of scalar | Rtuple of rep list

  (* It also contains the set of RVs this term owns, independently of its substructure *)
  type t = rep * RVSet.t

  let empty = Rscalar (RVSet.empty, RVSet.empty)

  (* Given two representations, conservatively represent a choice between them *)
  let rec join (rep1, own1) (rep2, own2) =
    let r =
      match (rep1, rep2) with
      | Rscalar (must1, may1), Rscalar (must2, may2) ->
          let must =
            RVSet.union (RVSet.inter must1 must2)
              (RVSet.union (RVSet.inter must1 own1) (RVSet.inter must2 own2))
          in
          Rscalar (must, RVSet.union may1 may2)
      | Rtuple rs1, Rtuple rs2 ->
          let rec join' rs1 rs2 =
            match (rs1, rs2) with
            | [], [] -> []
            | r1 :: rs1, r2 :: rs2 ->
                let r, _ = join (r1, own1) (r2, own2) in
                r :: join' rs1 rs2
            | _ ->
                failwith "Cannot join tuple representations of different length"
          in
          Rtuple (join' rs1 rs2)
      | _ -> failwith "Cannot join scalar and tuple representations"
    in
    (r, RVSet.union own1 own2)

  (* Given a scalar representation, extract its components *)
  let get = function
    | Rscalar (must, may) -> (must, may)
    | _ -> failwith "Unexpected tuple representation"

  (* Given a representation, merge it into a scalar and extract its components *)
  let rec fold = function
    | Rscalar (must, may) -> (must, may)
    | Rtuple rs ->
        List.fold_left
          (fun (must, may) r ->
            let must', may' = fold r in
            (RVSet.union must must', RVSet.union may may'))
          (RVSet.empty, RVSet.empty) rs
end

module Type = struct
  (* General muF types, including functions and streams. *)
  (* TODO: don't do this. Just add a context for m's. *)
  type ('p, 'e) t =
    | Trep of Rep.t
    | Tfun of 'p * 'e
    | Tstream of {
        t_state : ('p, 'e) t;
        p_state : 'p;
        p_in : 'p;
        ctx : ('p, 'e) t VarMap.t;
        e : 'e;
      }
    | Tbounded
end

module type Analysis = sig
  (* Abstract state kept by an analysis *)
  type t

  (* Update an abstract state by assuming an RV *)
  val assume : Rep.scalar * t -> int -> t

  (* Update an abstract state by observing an RV with an expression *)
  val observe : Rep.scalar * t -> int -> t

  (* Update an abstract state by evaluating an expression *)
  val value : Rep.scalar * t -> t

  (* Compute an abstract state that is a conservative choice between two states *)
  val join : t -> t -> t
end

let rec get_ctx ctx p rep =
  let open Rep in
  match (p, rep) with
  | Pid { name }, _ -> VarMap.add name rep ctx
  | Ptuple [], Rtuple [] -> ctx
  | Ptuple (p :: ps), Rtuple (r :: rs) ->
      get_ctx (get_ctx ctx p.patt r) (Ptuple ps) (Rtuple rs)
  | Ptype (p, _), _ -> get_ctx ctx p.patt rep
  | Pany, _ -> ctx
  | _, _ -> failwith "Representation and pattern mismatch"

(* Raised when an inner infer fails *)
exception Infer

module Evaluator (A : Analysis) = struct
  let new_var =
    let i = ref 0 in
    fun () ->
      let i' = !i + 1 in
      i := i';
      i'

  let eval (init : A.t) _check_infer ops funcs ctx e =
    let rec eval (ctx : Rep.rep VarMap.t) (state : A.t) e : Rep.t * A.t =
      match e with
      | Econst _ | Evar { name = "List.nil" } | Evar { name = "Array.empty" } ->
          ((Rep.empty, RVSet.empty), state)
      | Evar { name } ->
          let v =
            match VarMap.find_opt name ctx with Some v -> v | _ -> Rep.empty
          in
          ((v, RVSet.empty), state)
      | Esample (_, e) ->
          let v = new_var () in
          let (rep, own), state = eval ctx state e.expr in
          let state = A.assume (Rep.get rep, state) v in
          let s = RVSet.singleton v in
          ((Rscalar (s, s), RVSet.add v own), state)
      | Eobserve (_, e1, e2) ->
          let (rep, own1), state = eval ctx state e1.expr in
          let v = new_var () in
          let state = A.assume (Rep.get rep, state) v in
          let (rep', own2), state = eval ctx state e2.expr in
          let state = A.value (Rep.get rep', state) in
          let state = A.observe (Rep.get rep', state) v in
          ((Rtuple [], RVSet.add v (RVSet.union own1 own2)), state)
      | Efactor _ -> failwith "factor not implemented"
      | Eif (e, e1, e2) ->
          let (rep, own), state = eval ctx state e.expr in
          let state = A.value (Rep.get rep, state) in
          let (rep1, own1), state1 = eval ctx state e1.expr in
          let (rep2, own2), state2 = eval ctx state e2.expr in
          let rep, own' = Rep.join (rep1, own1) (rep2, own2) in
          ((rep, RVSet.union own own'), A.join state1 state2)
      | Eapp (e1, e2) -> (
          let rec clear_must = function
            | Rep.Rscalar (_, may) -> Rep.Rscalar (RVSet.empty, may)
            | Rep.Rtuple rs -> Rep.Rtuple (List.map clear_must rs)
          in
          let mk_expr e = { expr = e; emeta = () } in
          match e1.expr with
          | Evar { name = "Array.init" } | Evar { name = "List.init" } -> (
              let _, state = eval ctx state e2.expr in
              match e2.expr with
              | Etuple [ prob; _; f ] ->
                  let (rep, own), state =
                    eval ctx state
                      (Eapp
                         ( f,
                           mk_expr (Etuple [ prob; mk_expr (Econst (Cint 0)) ])
                         ))
                  in
                  ((clear_must rep, own), state)
              | _ -> failwith "init incorrect arguments")
          | Evar { name = "Array.get" } -> (
              let _, state = eval ctx state e2.expr in
              match e2.expr with
              | Etuple [ a; _ ] -> eval ctx state a.expr
              | _ -> failwith "Array.get incorrect arguments")
          | Evar { name = "List.append" } -> (
              match e2.expr with
              | Etuple [ x; xs ] ->
                  let r, state = eval ctx state x.expr in
                  let r', state = eval ctx state xs.expr in
                  (Rep.join r r', state)
              | _ -> failwith "List.append incorrect arguments")
          | Evar { name = "List.map" } -> (
              match e2.expr with
              | Etuple [ prob; f; l ] ->
                  let (arg, own'), state = eval ctx state l.expr in
                  let (rep, own), state =
                    eval (VarMap.add "arg" arg ctx) state
                      (Eapp
                         ( f,
                           mk_expr
                             (Etuple [ prob; mk_expr (Evar { name = "arg" }) ])
                         ))
                  in
                  ((clear_must rep, RVSet.union own own'), state)
              | _ -> failwith "List.map incorrect arguments")
          | Evar { name = "List.iter2" } -> (
              match e2.expr with
              | Etuple [ prob; f; l1; l2 ] ->
                  let (arg1, own1), state = eval ctx state l1.expr in
                  let (arg2, own2), state = eval ctx state l2.expr in
                  let (rep, own), state =
                    eval
                      (VarMap.add "arg1" arg1 (VarMap.add "arg2" arg2 ctx))
                      state
                      (Eapp
                         ( f,
                           mk_expr
                             (Etuple
                                [
                                  prob;
                                  mk_expr (Evar { name = "arg1" });
                                  mk_expr (Evar { name = "arg2" });
                                ]) ))
                  in
                  ( (clear_must rep, RVSet.union own (RVSet.union own1 own2)),
                    state )
              | _ -> failwith "List.iter2 incorrect arguments")
          | Evar { name = "List.filter" } -> (
              match e2.expr with
              | Etuple [ prob; f; l ] ->
                  let (arg, own), state = eval ctx state l.expr in
                  let (_, own'), state =
                    eval (VarMap.add "arg" arg ctx) state
                      (Eif
                         ( mk_expr
                             (Eapp
                                ( f,
                                  mk_expr
                                    (Etuple
                                       [ prob; mk_expr (Evar { name = "arg" }) ])
                                )),
                           mk_expr (Econst (Cbool true)),
                           mk_expr (Econst (Cbool false)) ))
                  in
                  ((clear_must arg, RVSet.union own own'), state)
              | _ -> failwith "List.filter incorrect arguments")
          | Evar { name = "List.length" } -> eval ctx state e2.expr
          | Evar { name = "eval" } ->
              let (arg, own), state = eval ctx state e2.expr in
              let state = A.value (Rep.get arg, state) in
              ((arg, own), state)
          | Evar { name } -> (
              let (arg, own'), state' = eval ctx state e2.expr in
              if List.mem name ops then ((Rscalar (Rep.fold arg), own'), state')
              else
                match VarMap.find_opt name funcs with
                | Some _ -> failwith "Unimplemented"
                | _ -> failwith ("Illegal operator " ^ name))
          | _ -> failwith "Illegal operator")
      | Elet (p, e, e') ->
          let (rep, own'), state = eval ctx state e.expr in
          let (rep, own), state = eval (get_ctx ctx p.patt rep) state e'.expr in
          ((rep, RVSet.union own' own), state)
      | Etuple es ->
          let (rep, own), state =
            List.fold_left
              (fun ((acc, own), state) e ->
                let (rep, own'), state = eval ctx state e.expr in
                ((rep :: acc, RVSet.union own own'), state))
              (([], RVSet.empty), state)
              es
          in
          ((Rtuple (List.rev rep), own), state)
      | Erecord _ -> failwith "Record not implemented"
      | Efield _ -> failwith "Record access not implemented"
      | Esequence _ -> failwith "Sequence not implemented"
      | Einfer (_, _) ->
          assert false
          (* XXX TODO XXX *)
          (* let (rep, own), state = eval ctx state e'.expr in *)
          (* if check_infer p e then ((rep, own), state) else raise Infer *)
      | Ecall_init _ | Ecall_step (_, _) | Ecall_reset _ -> assert false
      (* XXX TODO XXX *)
    in
    eval ctx init e
end

module Consumed = struct
  (* RVs introduced, RVs consumed *)
  type t = RVSet.t * RVSet.t

  let init = (RVSet.empty, RVSet.empty)

  let assume ((must, _), (add, rem)) v = (RVSet.add v add, RVSet.union rem must)

  let observe ((must, _), (add, rem)) v =
    (add, RVSet.union rem (RVSet.add v must))

  let value ((must, _), (add, rem)) = (add, RVSet.union rem must)

  let join (add1, rem1) (add2, rem2) =
    (RVSet.union add1 add2, RVSet.inter rem1 rem2)
end

module UnseparatedPaths = struct
  (* Path matrix, separator set *)
  type t = int RVMap.t RVMap.t * RVSet.t

  let init rep =
    let must, may = Rep.fold rep in
    let vs = RVSet.union must may in
    let p =
      RVSet.fold
        (fun v acc -> RVMap.add v (RVMap.singleton v 0) acc)
        vs RVMap.empty
    in
    (p, RVSet.empty)

  let assume ((_, may), ((p, sep) : t)) v : t =
    let update (v_p : int) (h : int RVMap.t) =
      match RVMap.find_opt v_p p with
      | None -> h
      | Some v_is -> RVMap.fold (fun v_i l h -> RVMap.add v_i (l + 1) h) v_is h
    in
    let p =
      RVMap.add v
        (RVSet.fold update (RVSet.diff may sep) (RVMap.singleton v 0))
        p
    in
    (p, sep)

  let observe ((must, _), (p, sep)) v = (p, RVSet.union sep (RVSet.add v must))

  let value ((must, _), (p, sep)) = (p, RVSet.union sep must)

  let path_union = RVMap.union (fun _ x y -> Some (max x y))

  let all_path_union = RVMap.union (fun _ x y -> Some (path_union x y))

  let join (p1, sep1) (p2, sep2) = (all_path_union p1 p2, RVSet.inter sep1 sep2)
end

let m_consumed ops funcs p e =
  let module C = Evaluator (Consumed) in
  let rec eval ctx e = C.eval Consumed.init check_infer ops funcs ctx e
  and check_infer p e =
    let (rep, _), (add, rem) = eval' e p Rep.empty in
    let _, may = Rep.fold rep in
    RVSet.is_empty (RVSet.inter (RVSet.diff add rem) may)
  and eval' e p rep = eval (get_ctx VarMap.empty p.patt rep) e.expr in
  let rep = Rep.empty in
  (rep, eval' e p rep)

let unseparated_paths ops funcs n_iters p e =
  let module UP = Evaluator (UnseparatedPaths) in
  let rec eval (p, sep) ctx e = UP.eval (p, sep) check_infer ops funcs ctx e
  and check_infer p e =
    match (p.patt, Rep.empty) with
    | Ptuple [ state_p; obs_p ], Rtuple [ state_rep; obs_rep ] ->
        let obs_ctx = get_ctx VarMap.empty obs_p.patt obs_rep in
        let rec run (p, sep) prev_state prev_max n_iters =
          n_iters > 0
          &&
          let (rep, _), (p, sep) =
            eval (p, sep) (get_ctx obs_ctx state_p.patt prev_state) e.expr
          in
          let _, may = Rep.fold rep in
          let new_max =
            RVMap.fold
              (fun _ srcs ->
                RVMap.fold
                  (fun src len acc ->
                    if RVSet.mem src may then max len acc else acc)
                  srcs)
              (RVMap.filter (fun v _ -> not (RVSet.mem v sep)) p)
              prev_max
          in
          new_max = prev_max
          ||
          match rep with
          | Rtuple [ _; new_state ] ->
              run (p, sep) new_state new_max (n_iters - 1)
          | _ -> failwith "step does not return output and new state"
        in
        run (UnseparatedPaths.init state_rep) state_rep Int.min_int n_iters
    | _ -> failwith "step and infer must specify state and input"
  in
  let rep = Rep.empty in
  let ctx = get_ctx VarMap.empty p.patt rep in
  let v = eval (UnseparatedPaths.init rep) ctx e.expr in
  (rep, v)
