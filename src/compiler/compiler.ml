open Format
exception Error

let output_loc ff (file, (p1, p2)) =
  fprintf ff "%s, characters %d to %d@." file p1 p2

let lexical_error loc err =
  eprintf "%aLexical error: %s.@." output_loc loc err;
  raise Error

let syntax_error loc =
  eprintf "%aSyntax error.@." output_loc loc;
  raise Error

let parse parsing_fun lexing_fun source_name =
  let ic = open_in source_name in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = source_name };
  try parsing_fun lexing_fun lexbuf with
  | Lexer.Lexical_error err ->
      let loc = (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) in
      close_in ic;
      lexical_error (source_name, loc) err
  | Parser.Error ->
      let loc = (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) in
      close_in ic;
      syntax_error (source_name, loc)

let analyze_file n_iters p =
  let module SMap = Map.Make (String) in
  let open Muf in
  List.fold_left
    (fun (fctx, mctx, (mcons, unsep)) d ->
      match d.decl with
      | Dfun ({ name }, p, e) ->
          let rs = Analysis.process_fn p e fctx mctx in
          (SMap.add name rs fctx, mctx, (mcons, unsep))
      | Dnode ({ name }, _, node) -> (
          let p, e = node.n_step in
          match p.patt with
          | Ptuple [ p_state; p_in ] ->
              let rs, (mcons', unsep') = 
                Analysis.process_node 
                  n_iters node.n_init p_state p_in e fctx mctx
              in
              ( fctx,
                SMap.add name rs mctx, (mcons && mcons', unsep && unsep'))
          | _ -> failwith "Stream definition lacking step (state, input).")
      | _ -> (fctx, mctx, (mcons, unsep)))
    (SMap.empty, SMap.empty, (true, true)) p
  |> (fun (_, _, (mcons, unsep)) -> 
        if mcons 
        then Format.printf "     ✓ : m-consumed analysis success@."
        else Format.printf "     ✗ : m-consumed analysis failure@.";
        if unsep 
        then Format.printf "     ✓ : Unseparated paths analysis success@."
        else Format.printf "     ✗ : Unseparated paths analysis failure@.")

let compile_file muf_list name =
  let mlc = open_out (name ^ ".ml") in
  let mlff = Format.formatter_of_out_channel mlc in
  try
    let ml_list = List.map Muf_gencode.compile_program [muf_list] in
    Format.fprintf mlff "%s@.%s@.%s@.%s@.%s@.@.%a@."
      "open Probzelus"
      "open Distribution"
      "open Muf"
      "open Infer_ds_streaming"
      "open Infer_muf"
      (pp_print_list ~pp_sep:pp_force_newline Pprintast.structure) ml_list;
  with Zmisc.Error -> close_out mlc; raise Error
  

let compile_simulator name node =
  let mainc = open_out (node ^ ".ml") in
  let mainff = Format.formatter_of_out_channel mainc in
  Format.fprintf mainff
    "@[<v> open Muf @;@;\
     @[(* simulation (discrete) function *)@]@;\
     @[<v 2>@[let main =@]@;\
     @[let mem = ref (Muflib.init %s.%s) in@]@;\
     @[(fun x -> let _, s = Muflib.step !mem x in mem := s)@]@]@];;@.\
     @[<v>(* (discrete) simulation loop *)@;\
         while true do main () done;@;\
         exit(1);;@]@."
    (String.capitalize_ascii name) node;
  close_out mainc
  
let print_cmd name node = 
  let cmd = 
    "ocamlfind ocamlc -linkpkg -package muf " ^ 
    name ^ ".ml " ^ node ^ ".ml " 
    ^ "-o " ^ name ^ "_" ^ node ^ ".exe"
  in
  Format.printf "%s@." cmd;
  match Sys.command cmd with
  | 0 -> ()
  | _ -> raise Error


let only_check = ref false
let simulation_node = ref "main"
let up_bound = ref 10

let compile file = 
  let name = Filename.chop_extension file in
  let node = !simulation_node in
  let muf_list = parse Parser.program (Lexer.token ()) file in
  Format.printf "-- Analyzing %s@." file;
  analyze_file !up_bound muf_list;
  if not !only_check then begin
    Format.printf "-- Generating %s.ml@." name;
    compile_file muf_list name;
    Format.printf "-- Generating %s.ml@." node;
    compile_simulator name node;
    print_cmd name node
  end