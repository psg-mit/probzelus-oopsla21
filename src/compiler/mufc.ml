open Mufcompilerlibs
open Format

let analyze_file p =
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
              Printf.printf "  Checking node %s:\n" name;
              ( fctx,
                SMap.add name
                  (Analysis.process_node node.n_init p_state p_in e fctx mctx)
                  mctx )
          | _ -> failwith "Stream definition lacking step (state, input).")
      | _ -> (fctx, mctx))
    (SMap.empty, SMap.empty) p
  |> ignore

let compile_file muf_list name =
  let ml_list = List.map Muf_gencode.compile_program [muf_list] in
  let mlc = open_out (name ^ ".ml") in
  let mlff = Format.formatter_of_out_channel mlc in
  Format.fprintf mlff "%s@.%s@.%s@.%s@.%s@.@.%a@."
    "open Probzelus"
    "open Distribution"
    "open Muf"
    "open Infer_ds_streaming"
    "open Infer_muf"
    (pp_print_list ~pp_sep:pp_force_newline Pprintast.structure) ml_list;
  close_out mlc

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
  ignore (Sys.command cmd)

let main file = 
  let name = Filename.chop_extension file in
  let node = !Misc.simulation_node in
  let muf_list = Misc.parse Parser.program (Lexer.token ()) file in
  Format.printf "-- Analyzing %s@." file;
  analyze_file muf_list;
  if not !Misc.only_check then begin
    Format.printf "-- Generating %s.ml@." name;
    compile_file muf_list name;
    Format.printf "-- Generating %s.ml@." node;
    compile_simulator name node;
    print_cmd name node
  end


let doc_simulation =
  "<node> \t Simulates the node <node> and generates a file <node>.ml"
let doc_only_check = "\t Only run the static analysis"


let () = 
  try 
    Arg.parse 
      (Arg.align [
        "--only-check", Arg.Unit Misc.set_only_check, doc_only_check;
        "--simulate", Arg.String Misc.set_simulation_node, doc_simulation;
      ])
      main 
      "Options are:"
  with Misc.Error -> exit 1