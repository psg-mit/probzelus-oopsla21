open Ast
module VarMap = Map.Make (String)
module RVSet = Set.Make (Int)
module RVMap = Map.Make (Int)

module Rep = struct
  (* Lower bound of used RVs, upper bound of used RVs, introduced RVs *)
  type scalar = RVSet.t * RVSet.t * RVSet.t

  (* The abstract representation of a uF term has the same shape as its type *)
  type t = Rscalar of scalar | Rtuple of t list

  let empty = Rscalar (RVSet.empty, RVSet.empty, RVSet.empty)

  (* Given two representations, conservatively represent a choice between them *)
  let rec join rep1 rep2 =
    match (rep1, rep2) with
    | Rscalar (must1, may1, intro1), Rscalar (must2, may2, intro2) ->
        let intro = RVSet.union intro1 intro2 in
        let must = RVSet.inter must1 must2 in
        let may = RVSet.union may1 may2 in
        Rscalar (RVSet.union intro must, RVSet.union intro may, intro)
    | Rtuple rs1, Rtuple rs2 ->
        let rec join' rs1 rs2 =
          match (rs1, rs2) with
          | [], [] -> []
          | r1 :: rs1, r2 :: rs2 -> join r1 r2 :: join' rs1 rs2
          | _ ->
              failwith "Cannot join tuple representations of different length"
        in
        Rtuple (join' rs1 rs2)
    | _ -> failwith "Cannot join scalar and tuple representations"

  (* Given a scalar representation, extract its components *)
  let get = function
    | Rscalar (must, may, intro) -> (must, may, intro)
    | _ -> failwith "Unexpected tuple representation"

  (* Given a representation, merge it into a scalar and extract its components *)
  let rec fold = function
    | Rscalar (must, may, intro) -> (must, may, intro)
    | Rtuple rs ->
        List.fold_left
          (fun (must, may, intro) r ->
            let must', may', intro' = fold r in
            ( RVSet.union must must',
              RVSet.union may may',
              RVSet.union intro intro' ))
          (RVSet.empty, RVSet.empty, RVSet.empty)
          rs
end

module type Analysis = sig
  (* Abstract state kept by an analysis *)
  type t

  (* Update an abstract state by assuming an RV *)
  val assume : Rep.scalar * t -> int -> t

  (* Update an abstract state by observing an expression with another *)
  val observe : Rep.scalar * t -> Rep.scalar -> t

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
    let rec eval (ctx : Rep.t VarMap.t) (state : A.t) e : Rep.t * A.t =
      match e with
      | Econst _ -> (Rep.empty, state)
      | Evar { name } -> (VarMap.find name ctx, state)
      | Esample e ->
          let v = new_var () in
          let rep, state = eval ctx state e.expr in
          let state = A.assume (Rep.get rep, state) v in
          let s = RVSet.singleton v in
          (Rscalar (s, s, s), state)
      | Eobserve (e1, e2) ->
          let rep, state = eval ctx state e1.expr in
          let v = new_var () in
          let state = A.assume (Rep.get rep, state) v in
          let rep', state = eval ctx state e2.expr in
          let state = A.value (Rep.get rep', state) in
          let s = RVSet.singleton v in
          let state = A.observe (Rep.get rep', state) (s, s, s) in
          (Rtuple [], state)
      | Eif (e, e1, e2) ->
          let rep, state = eval ctx state e.expr in
          let state = A.value (Rep.get rep, state) in
          let rep1, state1 = eval ctx state e1.expr in
          let rep2, state2 = eval ctx state e2.expr in
          (Rep.join rep1 rep2, A.join state1 state2)
      | Eapp (_, e2) ->
          let rep, state = eval ctx state e2.expr in
          (Rscalar (Rep.fold rep), state)
      | Elet (p, e, e') ->
          let rep, state = eval ctx state e.expr in
          let rec get_ctx ctx p rep =
            let open Rep in
            match (p, rep) with
            | Pid { name }, _ -> VarMap.add name rep ctx
            | Ptuple [], Rtuple [] -> ctx
            | Ptuple (p :: ps), Rtuple (r :: rs) ->
                get_ctx (get_ctx ctx p.patt r) (Ptuple ps) (Rtuple rs)
            | _, _ -> failwith "Representation and pattern mismatch"
          in
          eval (get_ctx ctx p.patt rep) state e'.expr
      | Etuple es ->
          let rep, state =
            List.fold_left
              (fun (acc, state) e ->
                let rep, state = eval ctx state e.expr in
                (rep :: acc, state))
              ([], state) es
          in
          (Rtuple (List.rev rep), state)
      | Einfer _ -> failwith "Infer not implemented"
    in
    eval ctx init e
end

module Consumed = struct
  (* RVs introduced, RVs consumed *)
  type t = RVSet.t * RVSet.t

  let init = (RVSet.empty, RVSet.empty)

  let assume ((must, _, _), (add, rem)) v =
    (RVSet.add v add, RVSet.union rem must)

  let observe ((must2, _, _), (add, rem)) (must1, _, _) =
    (add, RVSet.union rem (RVSet.union must1 must2))

  let value ((must, _, _), (add, rem)) = (add, RVSet.union rem must)

  let join (add1, rem1) (add2, rem2) =
    (RVSet.union add1 add2, RVSet.inter rem1 rem2)
end

module UnseparatedPaths = struct
  (* Path matrix, separator set *)
  type t = int RVMap.t RVMap.t * RVSet.t

  let init = (RVMap.empty, RVSet.empty)

  let assume ((must, _, _), ((p, sep) : t)) v : t =
    let update (v_p : int) (h : int RVMap.t) =
      match RVMap.find_opt v_p p with
      | None -> h
      | Some v_is -> RVMap.fold (fun v_i l h -> RVMap.add v_i (l + 1) h) v_is h
    in
    let p =
      RVMap.add v
        (RVSet.fold update (RVSet.diff must sep) (RVMap.singleton v 0))
        p
    in
    (p, sep)

  let observe ((must2, _, _), (p, sep)) (must1, _, _) =
    (p, RVSet.union sep (RVSet.union must1 must2))

  let value ((must, _, _), (p, sep)) = (p, RVSet.union sep must)

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
  let _, may, _ = Rep.fold rep in
  RVSet.is_empty (RVSet.inter (RVSet.diff add rem) may)

let unseparated_paths state_vars obs_vars n_iters f_init f_step =
  let module UP = Evaluator (UnseparatedPaths) in
  let rec run prev_state ctx prev_max n_iters =
    if n_iters = 0 then false
    else
      let rep, (p, sep) = UP.eval prev_state ctx f_step.expr in
      let _, may, _ = Rep.fold rep in
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
