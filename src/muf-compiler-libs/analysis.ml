open Muf
module VarMap = Map.Make (String)
module RVSet = Set.Make (Int)
module RVMap = Map.Make (Int)

module Rep = struct
  (* Lower bound of used RVs, upper bound of used RVs *)
  type scalar = RVSet.t * RVSet.t

  (* Streams track their current state, syntax, and contexts *)
  type ('p, 'e) stream = {
    t_state : ('p, 'e) rep;
    p_state : 'p;
    p_in : 'p;
    e : 'e;
    fctx : ('p, 'e) fn VarMap.t;
    mctx : ('p, 'e) stream VarMap.t;
  }

  (* Functions track their syntax and contexts *)
  and ('p, 'e) fn =
    | Fn of 'p * 'e * ('p, 'e) fn VarMap.t * ('p, 'e) stream VarMap.t

  (* The abstract representation of a uF term has the same rep as its type *)
  and ('p, 'e) rep =
    | Rscalar of scalar
    | Rtuple of ('p, 'e) rep list
    | Rstream of ('p, 'e) stream
    | Rbounded
    | Rmaybe of ('p, 'e) rep

  (* It also contains the set of RVs this term owns, independently of its substructure *)
  type ('p, 'e) t = ('p, 'e) rep * RVSet.t

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
      | Rmaybe r1, Rmaybe r2 ->
          let r, _ = join (r1, own1) (r2, own2) in
          Rmaybe r
      | _ -> failwith "Cannot join incompatible representations"
    in
    (r, RVSet.union own1 own2)

  (* Given a scalar representation, extract its components *)
  let get = function
    | Rscalar (must, may) -> (must, may)
    | _ -> failwith "Unexpected non-scalar representation"

  (* Given a representation, merge it into a scalar and extract its components *)
  let rec fold = function
    | Rscalar (must, may) -> (must, may)
    | Rtuple rs ->
        List.fold_left
          (fun (must, may) r ->
            let must', may' = fold r in
            (RVSet.union must must', RVSet.union may may'))
          (RVSet.empty, RVSet.empty) rs
    | Rstream m -> fold m.t_state
    | Rbounded -> (RVSet.empty, RVSet.empty)
    | Rmaybe r ->
        let _, may = fold r in
        (RVSet.empty, may)
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

let rec default p =
  match p.patt with
  | Pany | Pid _ -> Rep.empty
  | Ptuple ps -> Rep.Rtuple (List.map default ps)
  | Ptype (p, T_constr (("list" | "array"), [ _ ])) -> Rmaybe (default p)
  | Ptype (p, _) -> default p

let rec erase = function
  | Rep.Rscalar _ -> Rep.empty
  | Rtuple rs -> Rtuple (List.map erase rs)
  | Rstream m -> Rstream { m with t_state = erase m.t_state }
  | Rbounded -> Rbounded
  | Rmaybe r -> Rmaybe (erase r)

(* Raised when an inner infer fails *)
exception Infer

module Evaluator (A : Analysis) = struct
  let new_var =
    let i = ref 0 in
    fun () ->
      let i' = !i + 1 in
      i := i';
      i'

  let eval (init : A.t) check_infer ops ctx e =
    let rec eval ctx (state : A.t) e : ('p, 'e) Rep.t * A.t =
      match e with
      | Econst _ -> ((Rep.empty, RVSet.empty), state)
      | Evar { name = "Array.empty" } -> ((Rmaybe Rep.empty, RVSet.empty), state)
      | Evar { name = "List.nil2" } ->
          ((Rmaybe (Rtuple [ Rep.empty; Rep.empty ]), RVSet.empty), state)
      | Evar { name } ->
          let _, _, ctx = ctx in
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
          let mk_expr e = { expr = e; emeta = () } in
          let get_arg = function
            | Rep.Rmaybe r -> r
            | _ -> failwith "List incorrect arguments"
          in
          let rec clear_must = function
            | Rep.Rscalar (_, may) -> Rep.Rscalar (RVSet.empty, may)
            | Rep.Rtuple rs -> Rep.Rtuple (List.map clear_must rs)
            | Rep.Rmaybe r -> Rmaybe (clear_must r)
            | r -> r
          in
          match e1.expr with
          | Evar { name = "Array.init" } | Evar { name = "List.init" } -> (
              let _, state = eval ctx state e2.expr in
              match e2.expr with
              | Etuple [ _; f ] ->
                  let (rep, own), state =
                    eval ctx state
                      (Eapp (f, mk_expr (Etuple [ mk_expr (Econst (Cint 0)) ])))
                  in
                  ((Rmaybe (clear_must rep), own), state)
              | _ -> failwith "init incorrect arguments")
          | Evar { name = "Array.get" } -> (
              let _, state = eval ctx state e2.expr in
              match e2.expr with
              | Etuple [ a; _ ] ->
                  let (rep, own), state = eval ctx state a.expr in
                  ((get_arg rep, own), state)
              | _ -> failwith "Array.get incorrect arguments")
          | Evar { name = "List.append" } -> (
              match e2.expr with
              | Etuple [ l1; l2 ] ->
                  let (r, o1), state = eval ctx state l1.expr in
                  let (r', o2), state = eval ctx state l2.expr in
                  (Rep.join (r, o1) (r', o2), state)
              | _ -> failwith "List.append incorrect arguments")
          | Evar { name = "List.map" } -> (
              match e2.expr with
              | Etuple [ f; l ] ->
                  let (arg, own'), state = eval ctx state l.expr in
                  let fctx, mctx, ctx = ctx in
                  let (rep, own), state =
                    eval
                      (fctx, mctx, VarMap.add "arg" (get_arg arg) ctx)
                      state
                      (Eapp (f, mk_expr (Evar { name = "arg" })))
                  in
                  ((Rmaybe (clear_must rep), RVSet.union own own'), state)
              | _ -> failwith "List.map incorrect arguments")
          | Evar { name = "List.iter2" } -> (
              match e2.expr with
              | Etuple [ f; l1; l2 ] ->
                  let (arg1, own1), state = eval ctx state l1.expr in
                  let (arg2, own2), state = eval ctx state l2.expr in
                  let fctx, mctx, ctx = ctx in
                  let (rep, own), state =
                    eval
                      ( fctx,
                        mctx,
                        VarMap.add "arg1" (get_arg arg1)
                          (VarMap.add "arg2" (get_arg arg2) ctx) )
                      state
                      (Eapp
                         ( f,
                           mk_expr
                             (Etuple
                                [
                                  mk_expr (Evar { name = "arg1" });
                                  mk_expr (Evar { name = "arg2" });
                                ]) ))
                  in
                  ((rep, RVSet.union own (RVSet.union own1 own2)), state)
              | _ -> failwith "List.iter2 incorrect arguments")
          | Evar { name = "List.filter" } -> (
              match e2.expr with
              | Etuple [ f; l ] ->
                  let (arg, own), state = eval ctx state l.expr in
                  let fctx, mctx, ctx = ctx in
                  let (_, own'), state =
                    eval
                      (fctx, mctx, VarMap.add "arg" (get_arg arg) ctx)
                      state
                      (Eif
                         ( mk_expr (Eapp (f, mk_expr (Evar { name = "arg" }))),
                           mk_expr (Econst (Cbool true)),
                           mk_expr (Econst (Cbool false)) ))
                  in
                  ((arg, RVSet.union own own'), state)
              | _ -> failwith "List.filter incorrect arguments")
          | Evar { name = "List.length" } ->
              let (_, own), state = eval ctx state e2.expr in
              ((Rep.empty, own), state)
          | Evar { name = "eval" } ->
              let (arg, own), state = eval ctx state e2.expr in
              let state = A.value (Rep.get arg, state) in
              ((arg, own), state)
          | Evar { name } -> (
              let (arg, own'), state' = eval ctx state e2.expr in
              if List.mem name ops then ((Rscalar (Rep.fold arg), own'), state')
              else
                let fctx, _, _ = ctx in
                match VarMap.find_opt name fctx with
                | Some (Rep.Fn (p, e, fctx, mctx)) ->
                    eval
                      (fctx, mctx, get_ctx VarMap.empty p.patt arg)
                      state e.expr
                | None -> failwith ("Illegal operator " ^ name))
          | _ -> failwith "Illegal operator")
      | Elet (p, e, e') ->
          let (rep, own'), state = eval ctx state e.expr in
          let fctx, mctx, ctx = ctx in
          let (rep, own), state =
            eval (fctx, mctx, get_ctx ctx p.patt rep) state e'.expr
          in
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
      | Einfer (_, { name }) -> (
          let _, mctx, _ = ctx in
          match VarMap.find_opt name mctx with
          | None -> failwith ("Illegal stream " ^ name)
          | Some m ->
              if
                check_infer (erase m.t_state) m.p_state m.p_in m.e m.fctx m.mctx
              then ((Rbounded, RVSet.empty), state)
              else raise Infer)
      | Ecall_init e -> (
          match e.expr with
          | Evar { name } -> (
              let _, mctx, _ = ctx in
              match VarMap.find_opt name mctx with
              | None -> failwith ("Illegal stream " ^ name)
              | Some m -> ((Rep.Rstream m, RVSet.empty), state))
          | _ -> failwith "Can only init stream definitions")
      | Ecall_reset e ->
          let (rep, own), state = eval ctx state e.expr in
          let rep =
            match rep with
            | Rep.Rstream m -> Rep.Rstream { m with t_state = erase m.t_state }
            | Rep.Rbounded -> Rep.Rbounded
            | _ -> failwith "Expected stream"
          in
          ((rep, own), state)
      | Ecall_step (s, e) -> (
          let (rep, own), state = eval ctx state s.expr in
          let (arg, own'), state = eval ctx state e.expr in
          let own = RVSet.union own own' in
          match rep with
          | Rep.Rstream { t_state; p_state; p_in; e; fctx; mctx } ->
              let (out, own'), state =
                eval
                  ( fctx,
                    mctx,
                    get_ctx
                      (get_ctx VarMap.empty p_state.patt t_state)
                      p_in.patt arg )
                  state e.expr
              in
              let out =
                match out with
                | Rep.Rtuple [ t_out; t_state ] ->
                    Rep.Rtuple
                      [
                        t_out;
                        Rep.Rstream { t_state; p_state; p_in; e; fctx; mctx };
                      ]
                | _ -> failwith "Illegal stream step output"
              in
              ((out, RVSet.union own own'), state)
          | Rep.Rbounded -> ((Rep.empty, own), state)
          | _ -> failwith "Cannot unfold non-stream")
      | Esequence (e1, e2) ->
          eval ctx state (Elet ({ patt = Pany; pmeta = () }, e1, e2))
      | Erecord _ -> failwith "Record not implemented"
      | Efield _ -> failwith "Record access not implemented"
    in
    eval ctx init e
end

module Empty = struct
  type t = unit

  let init = ()

  let assume _ _ = ()

  let observe _ _ = ()

  let value _ = ()

  let join _ _ = ()
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

let ops =
  [
    "bernoulli";
    "gaussian";
    "beta";
    "delta";
    "infer_init";
    "ite";
    "plus";
    "sub";
    "mult";
    "eval";
    "get";
    "poisson";
    "shuffle";
    "not";
    "lt";
  ]

let initial_ctx t_init p_state p_in =
  get_ctx (get_ctx VarMap.empty p_state.patt t_init) p_in.patt (default p_in)

let m_consumed e fctx mctx =
  let module C = Evaluator (Consumed) in
  let rec eval ctx e = C.eval Consumed.init check_infer ops ctx e
  and check_infer t_init p_state p_in e fctx mctx =
    let (rep, _), (add, rem) =
      eval (fctx, mctx, initial_ctx t_init p_state p_in) e.expr
    in
    let _, may = Rep.fold rep in
    RVSet.is_empty (RVSet.inter (RVSet.diff add rem) may)
  in
  eval (fctx, mctx, VarMap.empty) e

let unseparated_paths n_iters e fctx mctx =
  let module UP = Evaluator (UnseparatedPaths) in
  let rec eval (p, sep) ctx e = UP.eval (p, sep) check_infer ops ctx e
  and check_infer t_init p_state p_in e fctx mctx =
    let in_ctx = get_ctx VarMap.empty p_in.patt (default p_in) in
    let rec run (p, sep) prev_state prev_max n_iters =
      n_iters > 0
      &&
      let (rep, _), (p, sep) =
        eval (p, sep)
          (fctx, mctx, get_ctx in_ctx p_state.patt prev_state)
          e.expr
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
      | Rtuple [ _; new_state ] -> run (p, sep) new_state new_max (n_iters - 1)
      | _ -> failwith "step does not return output and new state"
    in
    run (UnseparatedPaths.init t_init) t_init Int.min_int n_iters
  in
  eval (UnseparatedPaths.init Rep.empty) (fctx, mctx, VarMap.empty) e

let process_fn p e fctx mctx = Rep.Fn (p, e, fctx, mctx)

let process_node e_init p_state p_in e (fctx : ('p, 'e) Rep.fn VarMap.t)
    (mctx : ('p, 'e) Rep.stream VarMap.t) : ('p, 'e) Rep.stream =
  let module E = Evaluator (Empty) in
  let (t_init, _), _ =
    E.eval ()
      (fun _ _ _ _ _ _ -> true)
      ops (fctx, mctx, VarMap.empty) e_init.expr
  in
  let _ =
    try ignore (m_consumed e_init.expr fctx mctx)
    with Infer -> Printf.printf "m-consumed analysis failed\n"
  in
  let _ =
    try ignore (unseparated_paths 10 e_init.expr fctx mctx)
    with Infer -> Printf.printf "Unseparated paths analysis failed\n"
  in
  { t_state = t_init; p_state; p_in; e; fctx; mctx }
