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
  let c = Zlcompilerlibs.Muf_gencode.compile_program p in
  Format.printf "%a@." Pprintast.structure c;
  let f_init, f_step =
    List.fold_left
      Zlcompilerlibs.Muf.(
        fun (f_init, f_step) d ->
          match d.decl with
          | Dfun ({ name = "main_step" }, patt, e) -> (f_init, Some (e, patt))
          | Ddecl ({ patt = Pid { name = "main_init" }; _ }, e) ->
              (Some e, f_step)
          | _ -> (f_init, f_step))
      (None, None) p
  in
  match (f_init, f_step) with
  | Some _, Some (f_step, p) ->
      let ctx = Analysis.init_ctx p in
      let success =
        try
          ignore (Analysis.m_consumed ctx f_step);
          true
        with _ -> false
      in
      Format.printf "m-consumed: %B\n" success;
      let success =
        try
          ignore (Analysis.unseparated_paths 10 ctx f_step);
          true
        with _ -> false
      in
      Format.printf "unseparated_paths: %B\n" success
  | None, _ -> Format.printf "Missing main_init\n"
  | _, None -> Format.printf "Missing main_step\n"

let () = try Arg.parse [] compile_file "" with Error -> exit 1
