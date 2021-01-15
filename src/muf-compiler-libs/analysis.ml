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

  type mappers = (int -> RVSet.t) * (int -> RVSet.t)

  (* Given a placeholder rep and and an argument,
     get mapping functions from old variables to new variable sets *)
  let remap (rep, own) r new_var : mappers =
    let rec get_map (must_map, may_map) = function
      | Rscalar (v, _), Rscalar (must, may) ->
          RVSet.fold
            (fun v (must_map, may_map) ->
              (RVMap.add v must must_map, RVMap.add v may may_map))
            v (must_map, may_map)
      | Rtuple rs1, Rtuple rs2 ->
          let rec get_map' (must_map, may_map) (rs1, rs2) =
            match (rs1, rs2) with
            | [], [] -> (must_map, may_map)
            | r1 :: rs1, r2 :: rs2 ->
                get_map (get_map' (must_map, may_map) (rs1, rs2)) (r1, r2)
            | _ ->
                failwith
                  "Cannot subst tuple representations of different length"
          in
          get_map' (must_map, may_map) (rs1, rs2)
      | _ -> failwith "Cannot subst scalar and tuple representations"
    in
    let must_map, may_map = get_map (RVMap.empty, RVMap.empty) (rep, r) in
    let own_map =
      RVSet.fold (fun v acc -> RVMap.add v (new_var ()) acc) own RVMap.empty
    in
    let mapper m v =
      match RVMap.find_opt v own_map with
      | Some v -> RVSet.singleton v
      | None -> RVMap.find v m
    in
    (mapper must_map, mapper may_map)

  (* Given a representation parameterized by a placeholder, substitute components *)
  let subst (must_mapper, may_mapper) f own =
    let rec subst' = function
      | Rscalar (must, may) ->
          Rscalar (flat_map must_mapper must, flat_map may_mapper may)
      | Rtuple rs -> Rtuple (List.map subst' rs)
    in
    (subst' f, flat_map must_mapper own)
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

  (* Given a state transition parameterized by a placeholder and a previous state,
     substitute components *)
  val subst : Rep.mappers -> t -> t -> t
end

let rec get_ctx ctx p rep =
  let open Rep in
  match (p, rep) with
  | Pid { name }, _ -> VarMap.add name rep ctx
  | Ptuple [], Rtuple [] -> ctx
  | Ptuple (p :: ps), Rtuple (r :: rs) ->
      get_ctx (get_ctx ctx p.patt r) (Ptuple ps) (Rtuple rs)
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

  let rec placeholder p =
    let open Rep in
    match p.patt with
    | Pid _ ->
        let i = new_var () in
        Rscalar (RVSet.singleton i, RVSet.singleton i)
    | Ptuple ps -> Rtuple (List.map placeholder ps)
    | Pany -> Rep.empty

  let eval (init : A.t) check_infer ops funcs ctx e =
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
      | Efactor _ -> failwith "factor not implemented"
      | Eif (e, e1, e2) ->
          let (rep, own), state = eval ctx state e.expr in
          let state = A.value (Rep.get rep, state) in
          let (rep1, own1), state1 = eval ctx state e1.expr in
          let (rep2, own2), state2 = eval ctx state e2.expr in
          let rep, own' = Rep.join (rep1, own1) (rep2, own2) in
          ((rep, RVSet.union own own'), A.join state1 state2)
      | Eapp (e1, e2) -> (
          let (arg, own'), state' = eval ctx state e2.expr in
          match e1.expr with
          | Evar { name } -> (
              if List.mem name ops then ((Rscalar (Rep.fold arg), own'), state')
              else
                match VarMap.find_opt name funcs with
                | Some (placeholder, ((f, own), state)) ->
                    let mappers = Rep.remap (placeholder, own) arg new_var in
                    let rep, own = Rep.subst mappers f own in
                    let state = A.subst mappers state state' in
                    ((rep, RVSet.union own own'), state)
                | _ -> failwith "Illegal operator")
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
      | Einfer ((p, e), e') ->
          let (rep, own), state = eval ctx state e'.expr in
          if check_infer p e then ((rep, own), state) else raise Infer
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

  let subst (must_mapper, may_mapper) (f_add, f_rem) (a_add, a_rem) =
    let f_add = flat_map may_mapper f_add in
    let f_rem = flat_map must_mapper f_rem in
    (RVSet.union a_add f_add, RVSet.union a_rem f_rem)
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

  let subst (must_mapper, may_mapper) (f_p, f_sep) (a_p, a_sep) =
    let f_sep = flat_map must_mapper f_sep in
    let add_max p src dst l =
      RVMap.update dst
        (function
          | None -> Some (RVMap.singleton src l)
          | Some p ->
              Some
                (RVMap.update src
                   (function None -> Some l | Some l' -> Some (max l l'))
                   p))
        p
    in
    let f_p =
      RVMap.fold
        (fun dst p acc ->
          let dsts = may_mapper dst in
          RVMap.fold
            (fun src l acc ->
              let srcs = may_mapper src in
              RVSet.fold
                (fun src acc ->
                  RVSet.fold (fun dst acc -> add_max acc src dst l) dsts acc)
                srcs acc)
            p acc)
        f_p RVMap.empty
    in
    let p = all_path_union f_p a_p in
    let sep = RVSet.union f_sep a_sep in
    (p, sep)
end

let m_consumed ops funcs p e =
  let module C = Evaluator (Consumed) in
  let rec eval ctx e = C.eval Consumed.init check_infer ops funcs ctx e
  and check_infer p e =
    let (rep, _), (add, rem) = eval' e p (C.placeholder p) in
    let _, may = Rep.fold rep in
    RVSet.is_empty (RVSet.inter (RVSet.diff add rem) may)
  and eval' e p rep = eval (get_ctx VarMap.empty p.patt rep) e.expr in
  let rep = C.placeholder p in
  (rep, eval' e p rep)

let unseparated_paths ops funcs n_iters p e =
  let module UP = Evaluator (UnseparatedPaths) in
  let rec eval (p, sep) ctx e = UP.eval (p, sep) check_infer ops funcs ctx e
  and check_infer p e =
    match (p.patt, UP.placeholder p) with
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
        run UnseparatedPaths.init state_rep Int.min_int n_iters
    | _ -> failwith "step and infer must specify state and input"
  in
  let rep = UP.placeholder p in
  let v = eval UnseparatedPaths.init (get_ctx VarMap.empty p.patt rep) e.expr in
  (rep, v)
