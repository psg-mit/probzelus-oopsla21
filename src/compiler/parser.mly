%{
  open Muf

  let mk_expr e =
    { expr = e; emeta = (); }

  let mk_patt e =
    { patt = e; pmeta = (); }

%}

%token <bool> BOOL
%token <int> INT
%token <string> FLOAT
%token <string> STRING
%token <string> IDENT

%token OPEN
%token VAL LET IN FUN STREAM
%token IF THEN ELSE
%token FACTOR SAMPLE OBSERVE INFER INIT UNFOLD RESET
%token BOOLT INTT FLOATT DIST UNIT ARRAY LIST

%token DOT
%token EQUAL ARROW
%token LPAREN RPAREN LCURLY RCURLY
%token COMMA SEMI
%token EOF
%token COLON
%token STAR
%token UNDERSCORE

%start <unit Muf.program> program

%%

program:
| p = list(decl) EOF
    { p }

decl:
| OPEN m = IDENT
    { { decl = Dopen m } }
| VAL x = patt EQUAL e = expr
    { { decl = Ddecl (x, e) } }
(* Function *)
| VAL x = IDENT EQUAL FUN p = patt ARROW e = expr
    { { decl = Dfun (x, p, e) } }
| VAL x = IDENT EQUAL STREAM LCURLY INIT EQUAL e_init = expr SEMI step = IDENT p = patt EQUAL e_step = expr RCURLY
    { begin match step with "step" -> () | _ -> failwith "step expected" end;
      let n =
        { n_type = ([], TKrecord []); (* XXX TODO XXX *)
          n_init = e_init;
          n_step = (p, e_step); }
      in
      { decl = Dnode (x, [], n) } }

simple_expr:
(* Parenthesized expression *)
| LPAREN e = expr RPAREN
    { e }
(* Constants *)
| b = BOOL
    { mk_expr (Econst (Cbool b)) }
| i = INT
    { mk_expr (Econst (Cint i)) }
| f = FLOAT
    { mk_expr (Econst (Cfloat f)) }
| s = STRING
    { mk_expr (Econst (Cstring s)) }
(* Variable *)
| x = IDENT
    { mk_expr (Evar { modul = None; name = x }) }
| m = IDENT DOT x = IDENT
    { mk_expr (Evar { modul = Some m; name = x }) }
| m = IDENT DOT INIT
    { mk_expr (Evar { modul = Some m; name = "init" }) }
(* Unit *)
| LPAREN RPAREN { mk_expr (Etuple []) }
(* Tuple *)
| LPAREN e1 = simple_expr COMMA el = separated_nonempty_list(COMMA, simple_expr) RPAREN
    { mk_expr (Etuple (e1 :: el)) }
(* Call *)
| e1 = simple_expr LPAREN e2 = expr RPAREN
    { mk_expr (Eapp (e1, e2)) }
(* Call Tuple *)
| e1 = simple_expr LPAREN e2 = simple_expr COMMA el = separated_nonempty_list(COMMA, simple_expr) RPAREN
    { mk_expr (Eapp (e1, mk_expr (Etuple (e2 :: el)))) }
(* Probabilitic expressions *)
| FACTOR LPAREN e = expr RPAREN
    { mk_expr (Efactor ("prob", e)) }
| SAMPLE LPAREN e = expr RPAREN
    { mk_expr (Esample ("prob", e)) }
| OBSERVE LPAREN e1 = simple_expr COMMA e2 = simple_expr RPAREN
    { mk_expr (Eobserve ("prob", e1, e2)) }
| INFER LPAREN e = simple_expr COMMA m = IDENT RPAREN
    { mk_expr (Einfer (e, { modul = None; name = m })) }
(* Streams *)
| INIT LPAREN m = IDENT RPAREN
    { mk_expr (Ecall_init (mk_expr (Evar { modul = None; name = m }))) }
| UNFOLD LPAREN e1 = simple_expr COMMA e2 = simple_expr RPAREN
    { mk_expr (Ecall_step (e1, e2)) }
| RESET LPAREN e = simple_expr RPAREN
    { mk_expr (Ecall_reset e) }

expr:
| e = simple_expr
    { e }
(* Conditional *)
| IF e = expr THEN e1 = simple_expr ELSE e2 = simple_expr
    { mk_expr (Eif (e, e1, e2)) }
(* Local binding *)
| LET x = patt EQUAL e1 = expr IN e2 = expr
    { mk_expr (Elet (x, e1, e2)) }

patt:
| x = IDENT
    { mk_patt (Pid { modul = None; name = x }) }
| LPAREN p1 = patt COMMA pl = separated_nonempty_list(COMMA, patt) RPAREN
    { mk_patt (Ptuple (p1::pl)) }
| LPAREN RPAREN { mk_patt (Ptuple []) }
| x = IDENT COLON t = typ
    { mk_patt (Ptype (mk_patt (Pid { modul = None; name = x }), t))}
| UNDERSCORE { mk_patt Pany }

typ:
| INTT { Tany }
| FLOATT { Tany }
| BOOLT { Tany }
| t = typ DIST { Tconstr ("dist", [t]) }
| UNIT { Ttuple [] }
| LPAREN t = typ STAR tl = separated_nonempty_list(STAR, typ) RPAREN { Ttuple (t::tl) }
| UNDERSCORE { Tany }
| t = typ ARRAY { Tconstr ("array", [t]) }
| t = typ LIST { Tconstr ("list", [t]) }
