open Ast
module VarMap = Map.Make (String)
module RVSet = Set.Make (Int)
module RVMap = Map.Make (Int)

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

module Evaluator (A : Analysis) = struct
  let new_var =
    let i = ref 0 in
    fun () ->
      let i' = !i + 1 in
      i := i';
      i'

  let eval (init : A.t) ctx e =
    let rec eval (ctx : Rep.rep VarMap.t) (state : A.t) e : Rep.t * A.t =
      match e with
      | Econst _ -> ((Rep.empty, RVSet.empty), state)
      | Evar { name } -> ((VarMap.find name ctx, RVSet.empty), state)
      | Esample e ->
          let v = new_var () in
          let (rep, own), state = eval ctx state e.expr in
          let state = A.assume (Rep.get rep, state) v in
          let s = RVSet.singleton v in
          ((Rscalar (s, s), RVSet.add v own), state)
      | Eobserve (e1, e2) ->
          let (rep, own1), state = eval ctx state e1.expr in
          let v = new_var () in
          let state = A.assume (Rep.get rep, state) v in
          let (rep', own2), state = eval ctx state e2.expr in
          let state = A.value (Rep.get rep', state) in
          let state = A.observe (Rep.get rep', state) v in
          ((Rtuple [], RVSet.add v (RVSet.union own1 own2)), state)
      | Eif (e, e1, e2) ->
          let (rep, own), state = eval ctx state e.expr in
          let state = A.value (Rep.get rep, state) in
          let (rep1, own1), state1 = eval ctx state e1.expr in
          let (rep2, own2), state2 = eval ctx state e2.expr in
          let rep, own' = Rep.join (rep1, own1) (rep2, own2) in
          ((rep, RVSet.union own own'), A.join state1 state2)
      | Eapp (_, e2) ->
          let (rep, own), state = eval ctx state e2.expr in
          ((Rscalar (Rep.fold rep), own), state)
      | Elet (p, e, e') ->
          let (rep, own'), state = eval ctx state e.expr in
          let rec get_ctx ctx p rep =
            let open Rep in
            match (p, rep) with
            | Pid { name }, _ -> VarMap.add name rep ctx
            | Ptuple [], Rtuple [] -> ctx
            | Ptuple (p :: ps), Rtuple (r :: rs) ->
                get_ctx (get_ctx ctx p.patt r) (Ptuple ps) (Rtuple rs)
            | _, _ -> failwith "Representation and pattern mismatch"
          in
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
      | Einfer _ -> failwith "Infer not implemented"
    in
    let (rep, _), state = eval ctx init e in
    (rep, state)
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

  let init = (RVMap.empty, RVSet.empty)

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

let get_ctx init_rep state_vars obs_vars =
  let open Rep in
  let ctx =
    match init_rep with
    | Rscalar r -> (
        match state_vars with
        | [ name ] -> VarMap.singleton name (Rscalar r)
        | _ -> failwith "Program does not return entire state" )
    | Rtuple rs ->
        let rec f rs ns =
          match (rs, ns) with
          | [], [] -> VarMap.empty
          | r :: rs, n :: ns -> VarMap.add n r (f rs ns)
          | _ -> failwith "Program does not return correct state"
        in
        f rs state_vars
  in
  List.fold_left (fun ctx x -> VarMap.add x Rep.empty ctx) ctx obs_vars

let m_consumed state_vars obs_vars f_init f_step =
  let module C = Evaluator (Consumed) in
  let rep, (add, rem) = C.eval Consumed.init VarMap.empty f_init.expr in
  let add, rem = (RVSet.diff add rem, RVSet.empty) in
  let ctx = get_ctx rep state_vars obs_vars in
  let rep, (add, rem) = C.eval (add, rem) ctx f_step.expr in
  let _, may = Rep.fold rep in
  RVSet.is_empty (RVSet.inter (RVSet.diff add rem) may)

let unseparated_paths state_vars obs_vars n_iters f_init f_step =
  let module UP = Evaluator (UnseparatedPaths) in
  let rec run prev_state ctx prev_max n_iters =
    if n_iters = 0 then false
    else
      let rep, (p, sep) = UP.eval prev_state ctx f_step.expr in
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
      if new_max = prev_max then true
      else
        let ctx =
          match rep with
          | Rtuple [ _; r ] -> get_ctx r state_vars obs_vars
          | _ -> failwith "f_step does not return yielded value and new state"
        in
        run (p, sep) ctx new_max (n_iters - 1)
  in
  let rep, state = UP.eval UnseparatedPaths.init VarMap.empty f_init.expr in
  let ctx = get_ctx rep state_vars obs_vars in
  run state ctx Int.min_int n_iters
