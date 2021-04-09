open Format
open Muf_compiler_libs

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

let compile_file file =
  let p = parse Parser.program (Lexer.token ()) file in
  let module SMap = Map.Make (String) in
  let open Muf in
  List.fold_left
    (fun (fctx, mctx) d ->
      match d.decl with
      | Dfun ({ name }, p, e) ->
          (SMap.add name (Analysis.process_fn p e fctx mctx) fctx, mctx)
      | Dnode ({ name }, _, node) -> (
          let p, e = node.n_step in
          match p.patt with
          | Ptuple [ p_state; p_in ] ->
              ( fctx,
                SMap.add name (Analysis.process_node node.n_init p_state p_in e fctx mctx) mctx
              )
          | _ -> failwith "Stream definition lacking step (state, input).")
      | _ -> (fctx, mctx))
    (SMap.empty, SMap.empty) p
  |> ignore;
  Printf.printf "Analysis complete\n"

let () = try Arg.parse [] compile_file "" with Error -> exit 1
