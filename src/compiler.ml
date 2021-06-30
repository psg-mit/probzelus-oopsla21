open Muf_compiler_libs
open Format

let compile_file file =
  let filename = Filename.chop_extension file in
  let ml_name = filename ^ ".ml" in
  let muf_list = Misc.parse Parser.program (Lexer.token ()) file in
  let ml_list = List.map Muf_gencode.compile_program [muf_list] in
  let mlc = open_out ml_name in
  let mlff = Format.formatter_of_out_channel mlc in
    Format.fprintf mlff "%a@."
      (pp_print_list ~pp_sep:pp_force_newline Pprintast.structure) ml_list;
    close_out mlc

let () = try Arg.parse [] compile_file "" with Misc.Error -> exit 1