open Mufcompilerlibs
open Format

let compile_simulator name node =
  let mainc = open_out "main.ml" in
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

let compile_file file ml_name =
  let muf_list = Misc.parse Parser.program (Lexer.token ()) file in
  let ml_list = List.map Muf_gencode.compile_program [muf_list] in
  let mlc = open_out ml_name in
  let mlff = Format.formatter_of_out_channel mlc in
  Format.fprintf mlff "%s@.%s@.%s@.%s@.%s@.@.%a@."
    "open Probzelus"
    "open Distribution"
    "open Muf"
    "open Infer_ds_streaming"
    "open Infer_muf"
    (pp_print_list ~pp_sep:pp_force_newline Pprintast.structure) ml_list;
  close_out mlc
  
let print_cmd ml_name = 
  let cmd = 
    "ocamlfind ocamlc -linkpkg -package muf " ^ 
    ml_name ^ " main.ml -o main" 
  in
  Format.printf "To generate the executable, run@.  %s@." cmd

let main file = 
  let name = Filename.chop_extension file in
  let ml_name = name ^ ".ml" in
  compile_file file ml_name;
  Format.printf "Created %s@." ml_name;
  compile_simulator name "main";
  Format.printf "Created main.ml@.";
  print_cmd ml_name
  

let () = try Arg.parse [] main "" with Misc.Error -> exit 1