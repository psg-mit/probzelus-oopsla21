{
  open Parser
  exception Lexical_error of string

  let keyword_table =
    let tbl = Hashtbl.create 37 in
    begin
      List.iter (fun (key, data) -> Hashtbl.add tbl key data)
	[     ("val", VAL);
          ("let", LET);
	      ("in", IN);
          ("stream", STREAM);
          ("fun", FUN);
          ("if", IF);
          ("then", THEN);
          ("else", ELSE);
          ("factor", FACTOR);
          ("sample", SAMPLE);
          ("observe", OBSERVE);
          ("infer", INFER);
          ("true", BOOL true);
          ("false", BOOL false);
          ("bool", BOOLT);
          ("int", INTT);
          ("float", FLOATT);
          ("dist", DIST);
          ("unit", UNIT);
          ("array", ARRAY);
          ("list", LIST);
          ("init", INIT);
          ("unfold", UNFOLD);
          ("reset", RESET);
	]; tbl
    end

}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['A'-'Z' 'a'-'z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9' '.']

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* (frac exp? | exp)

rule token sbuff = parse
| eof { EOF }
| "=" { EQUAL }
| "->" { ARROW }
| "(" { LPAREN }
| ")" { RPAREN }
| "{" { LCURLY }
| "}" { RCURLY }
| "," { COMMA }
| ";" { SEMI }
| ":" { COLON }
| "*" { STAR }
| "_" { UNDERSCORE }
| [' ' '\t']
    { token sbuff lexbuf }
| newline
    { Lexing.new_line lexbuf; token sbuff lexbuf }
| float as f
    { FLOAT f }
| ('-'? ['0'-'9']+) as i
    { INT (int_of_string i) }
| '"'
    { STRING "XXX TODO XXX" }
    (* { reset_string sbuff; string sbuff lexbuf } *)
| letter identchar*
    { let s = Lexing.lexeme lexbuf in
      try Hashtbl.find keyword_table s
      with Not_found -> IDENT s }
| "(*"
    { comment 1 lexbuf; token sbuff lexbuf }
| _
    { raise (Lexical_error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

(* and string sbuff = parse *)
(*   | "\"\"" { add_char_to_string sbuff '"'; string sbuff lexbuf }                         (\* Escaped quote *\) *)
(*   | "\013\n" { add_char_to_string sbuff '\n'; string sbuff lexbuf } *)
(*   | "\013" { add_char_to_string sbuff '\n'; string sbuff lexbuf } *)
(*   | '"'    { let s = get_string sbuff in STRING s }  (\* End of string *\) *)
(*   | eof    { raise (Lexical_error "String not terminated.\n") } *)
(*   | _      { add_char_to_string sbuff (Lexing.lexeme_char lexbuf 0); string sbuff lexbuf } *)

and comment cpt = parse
  | "(*"
      { comment (cpt + 1) lexbuf }
  | "*)"
      { if cpt > 1 then comment (cpt - 1) lexbuf }
  | eof
      { raise (Lexical_error "Unterminated comment\n") }
  | newline
      { Lexing.new_line lexbuf; comment cpt lexbuf }
  | _
      { comment cpt lexbuf }

