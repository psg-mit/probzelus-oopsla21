open Muf_compiler_libs
open Format

let compile_file file =
  let name = Filename.chop_extension file in
  let ml_name = name ^ ".ml" in
  let muf_list = Misc.parse Parser.program (Lexer.token ()) file in
  let ml_list = List.map Muf_gencode.compile_program [muf_list] in
  let mlc = open_out ml_name in
  let mlff = Format.formatter_of_out_channel mlc in
  Format.fprintf mlff "%s@.%s@.%s@.@.%a@."
    "open Probzelus"
    "open Distribution"
    "open Infer_ds_streaming"
    (pp_print_list ~pp_sep:pp_force_newline Pprintast.structure) ml_list;
  close_out mlc;
  Format.printf "Created %s@." ml_name;
  let mainc = open_out "main.ml" in
  let mainff = Format.formatter_of_out_channel mainc in
  Format.fprintf mainff
    "@[<v>@[(* simulation (discrete) function *)@]@;\
     @[<v 2>@[let main =@]@;\
     @[let mem = ref (Muflib.init %s.main) in@]@;\
     @[(fun x -> let s, _ = Muflib.step !mem x in mem := s)@]@]@];;@.\
     @[<v>(* (discrete) simulation loop *)@;\
         while true do main () done;@;\
         exit(1);;@]@."
    (String.capitalize_ascii name);
  close_out mainc;
  Format.printf "Created main.ml@.";
  let cmd = 
    "ocamlfind ocamlc -linkpkg -package probzelus " ^ 
    ml_name ^ " main.ml -o main" 
  in
  Format.printf "To generate the executable, run@.  %s@." cmd

let () = try Arg.parse [] compile_file "" with Misc.Error -> exit 1