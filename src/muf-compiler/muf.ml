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
  let ops =
    [
      "bernoulli";
      "gaussian";
      "beta";
      "delta";
      "infer_init";
      "ite";
      "plus";
      "sub";
      "mult";
      "eval";
      "get";
      "poisson";
      "shuffle";
      "not";
      "lt";
    ]
  in
  List.fold_left
    Muf.(
      fun (mcs, ups) d ->
        match d.decl with
        | Dfun ({ name = "main_step" }, p, e) ->
            let success =
              try
                ignore (Analysis.m_consumed ops mcs p e);
                true
              with Analysis.Infer -> false
            in
            Format.printf "m-consumed: %B\n" success;
            let success =
              try
                ignore (Analysis.unseparated_paths ops ups 10 p e);
                true
              with Analysis.Infer -> false
            in
            Format.printf "unseparated_paths: %B\n" success;
            exit 0
        | Dfun ({ name }, p, e) ->
            let mc =
              try Analysis.m_consumed ops mcs p e
              with Analysis.Infer ->
                Format.printf "m-consumed failed for %s\n" name;
                exit 1
            in
            let up =
              try Analysis.unseparated_paths ops ups 10 p e
              with Analysis.Infer ->
                Format.printf "unseparated_paths failed for %s\n" name;
                exit 1
            in
            (SMap.add name mc mcs, SMap.add name up ups)
        | _ -> (mcs, ups))
    (SMap.empty, SMap.empty) p
  |> ignore

let () = try Arg.parse [] compile_file "" with Error -> exit 1
