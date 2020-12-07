type constant =
  | Cbool of bool
  | Cint of int
  | Cfloat of string
  | Cstring of string
[@@deriving show]

type identifier =
  { name: string }
[@@deriving show, map, fold]

type 'p patt_desc =
  | Pid of identifier
  | Ptuple of 'p list
[@@deriving show, map, fold]

type 'm pattern =
  { patt: 'm pattern patt_desc; pmeta: 'm }
[@@deriving show, map, fold]

type ('pattern, 'expr) expr_desc =
  | Econst of constant
  | Evar of identifier
  | Etuple of 'expr list
  | Eapp of 'expr * 'expr
  | Eif of 'expr * 'expr * 'expr
  | Elet of 'pattern * 'expr * 'expr
  | Esample of 'expr
  | Eobserve of 'expr * 'expr
  | Einfer of ('pattern * 'expr) * 'expr
[@@deriving show, map, fold]

type 'm expression =
  { expr: ('m pattern, 'm expression) expr_desc; emeta: 'm }
[@@deriving show, map, fold]

type ('p, 'e) decl_desc =
  | Ddecl of 'p * 'e
  | Dfun of 'p * 'p * 'e
[@@deriving show, map, fold]

type 'm declaration =
  { decl: ('m pattern, 'm expression) decl_desc }
[@@deriving show, map, fold]

type 'm program =
   'm declaration list
[@@deriving show]
