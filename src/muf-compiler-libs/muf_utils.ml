open Ast

module SSet = Set.Make(String)

let rec fv_patt patt =
  begin match patt.patt with
  | Pid x -> SSet.singleton x.name
  | Ptuple l ->
      List.fold_left (fun acc p -> SSet.union (fv_patt p) acc) SSet.empty l
  | Pany -> SSet.empty
  end

let rec fv_expr expr =
  begin match expr.expr with
  | Econst _ -> SSet.empty
  | Evar x -> SSet.singleton x.name
  | Etuple l ->
      List.fold_left (fun acc e -> SSet.union (fv_expr e) acc) SSet.empty l
  | Erecord (l, oe) ->
      List.fold_left
        (fun acc (_, e) -> SSet.union (fv_expr e) acc)
        (Option.fold ~none:SSet.empty ~some:fv_expr oe) l
  | Efield (e, _) -> fv_expr e
  | Eapp (e1, e2) -> SSet.union (fv_expr e1) (fv_expr e2)
  | Eif (e, e1, e2) ->
      SSet.union (fv_expr e) (SSet.union (fv_expr e1) (fv_expr e2))
  | Elet (p, e1, e2) ->
      SSet.union (fv_expr e1) (SSet.diff (fv_expr e2) (fv_patt p))
  | Esequence (e1, e2) -> SSet.union (fv_expr e1) (fv_expr e2)
  | Esample e -> fv_expr e
  | Eobserve (e1, e2) -> SSet.union (fv_expr e1) (fv_expr e2)
  | Efactor e -> fv_expr e
  | Einfer ((p, body), e) ->
      SSet.union (SSet.diff (fv_expr body) (fv_patt p)) (fv_expr e)
  end

let is_value expr =
  let rec is_value b expr =
    match expr.expr with
    | Eapp _ | Esample _ | Eobserve _ | Einfer _ -> false
    | e -> b && fold_expr_desc (fun b _ -> b) is_value b e
  in
  is_value true expr
