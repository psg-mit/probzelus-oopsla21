open Mufcompilerlibs
open Compiler


let set_only_check () = only_check := true
let set_simulation_node (n:string) = simulation_node := n
let set_up_bound (b: int) = up_bound := b


let () = 
  try 
    Arg.parse 
      (Arg.align [
        "--only-check", 
          Arg.Unit set_only_check, 
          "\t Only run the static analysis (default false)";
        "--simulate", 
          Arg.String set_simulation_node, 
          "<node> \t Simulates the node <node> and generates a file <node>.ml (default main)";
        "--up-bound",
          Arg.Int set_up_bound,
          "<int> \t iteration bound for the unseparated paths analysis (default 10)" 
      ])
      compile 
      "The muF Compiler. Options are:"
  with Error -> exit 2