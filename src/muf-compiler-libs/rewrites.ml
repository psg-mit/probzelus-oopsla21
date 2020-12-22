open Ast

let eunit m = { expr = Econst Cunit; emeta = m }

let is_value expr =
  let rec is_value b expr =
    match expr.expr with
    | Eapp _ | Esample _ | Eobserve _ | Einfer _ -> false
    | e -> b && fold_expr_desc (fun b _ -> b) is_value b e
  in
  is_value true expr

let rec subst x expr1 expr2 =
  match expr2.expr with
  | Evar y -> if x = y then expr1 else expr2
  | Elet ({ patt = Pid y; _ } as p, e1, e2) when x = y ->
      { expr2 with expr = Elet(p, subst x expr1 e1, e2) }
  | _ ->
      let desc = map_expr_desc (fun p -> p) (subst x expr1) expr2.expr in
      { expr2 with expr = desc }

let rec constant_propagation expr =
  match map_expr_desc (fun p -> p) constant_propagation expr.expr with
  | Elet ({ patt = Pid x; _ }, e1, e2) as e ->
      if is_value e1 then subst x e1 e2
      else { expr with expr = e }
  | e -> { expr with expr = e }


let rec eq_patt_expr patt expr =
  match patt.patt, expr.expr with
  | Pid x, Evar y -> x = y
  | Pid _, _ -> false
  | Pany, _ -> false
  | Ptuple pl, Etuple el -> List.for_all2 eq_patt_expr pl el
  | Ptuple _, _ -> false


let rec simplify_let patt expr =
  match patt.patt, expr.expr with
  | Pany, _ when is_value expr -> None
  | Ptuple pl, Etuple el ->
      let pel =
        List.fold_right2
          (fun p e acc ->
             match simplify_let p e with
             | None -> acc
             | Some (p, e) -> (p, e)::acc)
          pl el []
      in
      begin match pel with
      | [] -> None
      | [p1, e1] -> Some (p1, e1)
      |  _ ->
          let pl, el = List.split pel in
          Some ({ patt with patt = Ptuple pl }, { expr with expr = Etuple el })
      end
  | _ -> if eq_patt_expr patt expr then None else Some (patt, expr)

let rec simplify_lets expr =
  match map_expr_desc (fun p -> p) simplify_lets expr.expr with
  | Elet(p, e1, e2) ->
      begin match simplify_let p e1 with
      | None -> e2
      | Some (p, e1) -> { expr with expr = Elet(p, e1, e2) }
      end
  | e -> { expr with expr = e }
