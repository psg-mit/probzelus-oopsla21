open Ast


let () =
  let p = { patt = Pid { name = "x" }; pmeta = () } in
  let e = { expr = Econst (Cint 42); emeta = () } in
  let d = { decl = Ddecl (p, e) } in
  let p = [ d ] in
  let c = Compiler.compile_program p in
  Format.printf "%a@." Pprintast.structure c
