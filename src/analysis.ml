open Ast
module VarMap = Map.Make (String)
module RVSet = Set.Make (Int)

let new_var =
  let next_var = ref 0 in
  fun () ->
    let i = !next_var in
    next_var := i + 1;
    i

(* Given ctx mapping program vars to referenced RVs, and expression, 
   compute (referenced RVs, consumed input RVs, introduced output RVs) *)
let rec vars (ctx : RVSet.t VarMap.t) = function
  | Econst _ -> (RVSet.empty, RVSet.empty, RVSet.empty)
  | Evar { name } -> (VarMap.find name ctx, RVSet.empty, RVSet.empty)
  | Esample e -> (RVSet.empty, eval ctx e.expr, RVSet.singleton (new_var ()))
  | Eobserve (e1, e2) ->
      ( RVSet.empty,
        RVSet.union (eval ctx e1.expr) (eval ctx e2.expr),
        RVSet.empty )
  | Eif (e, e1, e2) ->
      let r1, i1, o1 = vars ctx e1.expr in
      let r2, i2, o2 = vars ctx e2.expr in
      ( RVSet.inter r1 r2,
        RVSet.union (eval ctx e.expr) (RVSet.inter i1 i2),
        RVSet.union o1 o2 )
  | Elet (p, e1, e2) ->
      let r, i, o = vars ctx e1.expr in
      let r, i', o' =
        match p.patt with
        | Pid { name } -> vars (VarMap.add name (RVSet.union r o) ctx) e2.expr
        | Ptuple [] -> vars ctx e2.expr
        | _ -> failwith "Tuple bindings are unimplemented"
      in
      (r, RVSet.union i i', RVSet.union o' (RVSet.diff o i'))
  | Etuple es ->
      List.fold_left
        (fun (s1, s2, s3) (x, y, z) ->
          (RVSet.union x s1, RVSet.union y s2, RVSet.union z s3))
        (RVSet.empty, RVSet.empty, RVSet.empty)
        (List.map (fun e -> vars ctx e.expr) es)
  | Eapp (_, e2) -> vars ctx e2.expr
  | Einfer _ -> failwith "Infer is unimplemented"

and eval ctx e =
  let r, i, o = vars ctx e in
  RVSet.union r (RVSet.union i o)

let m_consumed state_vars observations e =
  let ctx =
    List.fold_left
      (fun ctx x -> VarMap.add x RVSet.empty ctx)
      VarMap.empty
      (state_vars @ observations)
  in
  let referenced, input, output = vars ctx e.expr in
  (* TODO: state update, remove all dead variables *)
  let print_set s =
    s |> RVSet.elements |> List.map Int.to_string |> fun s ->
    "{" ^ String.concat " " s ^ "}"
  in
  Printf.printf "referenced: %s, input: %s, output: %s\n" (print_set referenced)
    (print_set input) (print_set output);
  RVSet.is_empty (RVSet.diff output input)
