open Format

(*
open Ast
let () =
  let p =
    {
      patt =
        Ptuple
          [
            {
              patt =
                Ptuple
                  [
                    { patt = Pid { name = "x_0" }; pmeta = () };
                    { patt = Pid { name = "x" }; pmeta = () };
                  ];
              pmeta = ();
            };
            { patt = Pid { name = "obs" }; pmeta = () };
          ];
      pmeta = ();
    }
  in
  let g =
    {
      expr =
        Eapp
          ( { expr = Evar { name = "gaussian" }; emeta = () },
            {
              expr =
                Etuple
                  [
                    { expr = Evar { name = "x" }; emeta = () };
                    { expr = Econst (Cfloat "1."); emeta = () };
                  ];
              emeta = ();
            } );
      emeta = ();
    }
  in
  let e =
    {
      expr =
        Efun
          ( p,
            {
              expr =
                Elet
                  ( { patt = Pid { name = "x" }; pmeta = () },
                    { expr = Esample g; emeta = () },
                    {
                      expr =
                        Elet
                          ( { patt = Pid { name = "y" }; pmeta = () },
                            g,
                            {
                              expr =
                                Elet
                                  ( { patt = Ptuple []; pmeta = () },
                                    {
                                      expr =
                                        Eobserve
                                          ( {
                                              expr = Evar { name = "y" };
                                              emeta = ();
                                            },
                                            {
                                              expr = Evar { name = "obs" };
                                              emeta = ();
                                            } );
                                      emeta = ();
                                    },
                                    {
                                      expr =
                                        Etuple
                                          [
                                            {
                                              expr = Evar { name = "x_0" };
                                              emeta = ();
                                            };
                                            {
                                              expr =
                                                Etuple
                                                  [
                                                    {
                                                      expr =
                                                        Evar { name = "x_0" };
                                                      emeta = ();
                                                    };
                                                    {
                                                      expr = Evar { name = "x" };
                                                      emeta = ();
                                                    };
                                                  ];
                                              emeta = ();
                                            };
                                          ];
                                      emeta = ();
                                    } );
                              emeta = ();
                            } );
                      emeta = ();
                    } );
              emeta = ();
            } );
      emeta = ();
    }
  in
  let alloc =
    {
      expr =
        Elet
          ( { patt = Pid { name = "x_0" }; pmeta = () },
            {
              expr =
                Esample
                  {
                    expr =
                      Eapp
                        ( { expr = Evar { name = "gaussian" }; emeta = () },
                          {
                            expr =
                              Etuple
                                [
                                  { expr = Econst (Cfloat "0."); emeta = () };
                                  { expr = Econst (Cfloat "1.0"); emeta = () };
                                ];
                            emeta = ();
                          } );
                    emeta = ();
                  };
              emeta = ();
            },
            {
              expr =
                Etuple
                  [
                    { expr = Evar { name = "x_0" }; emeta = () };
                    { expr = Evar { name = "x_0" }; emeta = () };
                  ];
              emeta = ();
            } );
      emeta = ();
    }
  in
  let p =
    [
      { decl = Ddecl ({ patt = Pid { name = "f_init" }; pmeta = () }, alloc) };
      { decl = Ddecl ({ patt = Pid { name = "f_step" }; pmeta = () }, e) };
    ]
  in
  let c = Compiler.compile_program p in
  Format.printf "%a@." Pprintast.structure c
*)

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
  Format.printf "%a@." Pprintast.structure c

let () =
  try
    Arg.parse
      []
      compile_file
      ""
  with Error ->
    exit 1

