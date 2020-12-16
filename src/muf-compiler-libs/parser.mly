%{
  open Ast

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

%token VAL LET IN FUN
%token IF THEN ELSE
%token SAMPLE OBSERVE INFER

%token EQUAL ARROW
%token LPAREN RPAREN
%token COMMA
%token EOF


%start <unit Ast.program> program

%%

program:
| p = list(decl) EOF
    { p }

decl:
| VAL x = patt EQUAL e = expr
    { { decl = Ddecl (x, e) } }
(* Function *)
| VAL x = IDENT EQUAL FUN p = patt ARROW e = expr
    { { decl = Dfun ({ name = x }, p, e) } }

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
    { mk_expr (Evar { name = x }) }
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
| SAMPLE LPAREN e = expr RPAREN
    { mk_expr (Esample e) }
| OBSERVE LPAREN e1 = expr COMMA e2 = expr RPAREN
    { mk_expr (Eobserve(e1, e2)) }
| INFER LPAREN FUN p = patt ARROW e1 = expr COMMA e2 = expr RPAREN
    { mk_expr (Einfer ((p, e1), e2))}

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
    { mk_patt (Pid { name = x }) }
| LPAREN p1 = patt COMMA pl = separated_nonempty_list(COMMA, patt) RPAREN
    { mk_patt (Ptuple (p1::pl)) }
| LPAREN RPAREN { mk_patt (Ptuple []) }