open Format

exception Error

let output_loc ff (file, (p1, p2)) =
  fprintf ff "%s, characters %d to %d@."
    file p1 p2

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
  try
    parsing_fun lexing_fun lexbuf
  with
  | Lexer.Lexical_error(err) ->
      let loc = (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) in
      close_in ic; lexical_error (source_name, loc) err
  | Parser.Error ->
      let loc = (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) in
      close_in ic;
      syntax_error (source_name, loc)

let compile_file file =
  let p = parse Parser.program (Lexer.token ()) file in
  let c = Compiler.compile_program p in
  Format.printf "%a@." Pprintast.structure c;
  (* TODO: also analyze f_init *)
  List.iter
    Ast.(
      fun d ->
        match d.decl with
        | Dfun
            ( { patt = Pid { name = "f_step" }; _ },
              { patt = Ptuple [ args; obs ]; _ },
              e ) ->
            let rec get p =
              match p.patt with
              | Pid { name } -> [ name ]
              | Ptuple ps -> List.concat (List.map get ps)
            in
            Format.printf "m-consumed: %B" (Analysis.m_consumed (get args) (get obs) e)
        | _ -> ())
    p

    let () =
    try
      Arg.parse
        []
        compile_file
        ""
    with Error ->
      exit 1

