open Mufcompilerlibs

let compile_file file =
  let _ = Printf.printf "Processing %s\n" file in
  let p = Misc.parse Parser.program (Lexer.token ()) file in
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
              let _ = Printf.printf "Checking node %s\n" name in
              ( fctx,
                SMap.add name
                  (Analysis.process_node node.n_init p_state p_in e fctx mctx)
                  mctx )
          | _ -> failwith "Stream definition lacking step (state, input).")
      | _ -> (fctx, mctx))
    (SMap.empty, SMap.empty) p
  |> ignore;
  Printf.printf "Analysis complete\n"

let () = try Arg.parse [] compile_file "" with Misc.Error -> exit 1
