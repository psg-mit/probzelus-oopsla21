open Ast_helper
open Ast

let with_loc: type a. a -> a with_loc = begin
  fun x ->
    { txt = x;
      loc = Location.none; }
end

let compile_const: constant -> Parsetree.expression = begin
  fun c ->
    begin match c with
    | Cbool x ->
        let b =
          with_loc (Longident.Lident (string_of_bool x))
        in
        Exp.construct b None
    | Cint x -> Exp.constant (Const.int x)
    | Cfloat x -> Exp.constant (Const.float x)
    | Cstring x -> Exp.constant (Const.string x)
    end
end

let rec compile_patt: type a. a pattern -> Parsetree.pattern = begin
  fun p ->
    begin match p.patt with
    | Pid x -> Pat.var (with_loc x.name)
    | Ptuple l -> Pat.tuple (List.map compile_patt l)
    end
end

let rec compile_expr:
  type a. ?inferlib: string -> a expression -> Parsetree.expression = begin
  fun ?(inferlib="Infer_pf") e ->
    begin match e.expr with
    | Econst c -> compile_const c
    | Evar x -> Exp.ident (with_loc (Longident.Lident x.name))
    | Etuple l -> Exp.tuple (List.map compile_expr l)
    | Eapp (e1, e2) -> Exp.apply (compile_expr e1) [Nolabel, compile_expr e2]
    | Eif (e, e1, e2) ->
        Exp.ifthenelse (compile_expr e)
          (compile_expr e1) (Some (compile_expr e2))
    | Elet (p, e1, e2) ->
        Exp.let_ Nonrecursive
          [ { Parsetree.pvb_pat = compile_patt p;
   	      pvb_expr = compile_expr e1;
   	      pvb_attributes = [];
   	      pvb_loc = Location.none; } ]
          (compile_expr e2)
    | Esample e ->
        let sample_id =
          Longident.Ldot (Longident.Lident inferlib, "sample")
        in
        Exp.apply (Exp.ident (with_loc sample_id)) [Nolabel, compile_expr e]
    | Eobserve (e1, e2) ->
        let observe_id =
          Longident.Ldot (Longident.Lident inferlib, "observe")
        in
        Exp.apply (Exp.ident (with_loc observe_id))
          [ (Nolabel, compile_expr e1); (Nolabel, compile_expr e2) ]
    | Einfer ((p, e), args) ->
        let infer_id =
          Longident.Ldot (Longident.Lident inferlib, "infer")
        in
        Exp.apply (Exp.ident (with_loc infer_id))
          [ (Nolabel, Exp.fun_ Nolabel None (compile_patt p) (compile_expr e));
            (Nolabel, compile_expr args) ]
    end
end

let compile_decl : type a. a declaration -> Parsetree.value_binding = begin
  fun d -> 
    match d.decl with
    | Ddecl (p, e) ->
      Vb.mk (compile_patt p) (compile_expr e)
    | Dfun (p1, p2, e) ->
      Vb.mk (compile_patt p1) (Exp.fun_ Nolabel None (compile_patt p2) (compile_expr e))
end


let compile_program : type a. a program -> Parsetree.structure = begin
  fun p ->
    List.map
      (fun decl ->
         Str.value Nonrecursive [ compile_decl decl ])
      p
end
