
let ocaml_puts name =
    Printf.printf "Program name is '%s'.\n" name ;
    (* Must flush stdout before returning to C. *)
    flush stdout

let ocaml_string_join join arr =
    (* Create and return a string. *)
    String.concat join (Array.to_list arr)

(* On program initialisation, register functions to be called from C. *)
let () =
    Callback.register "ocaml_puts" ocaml_puts ;
    Callback.register "ocaml_string_join" ocaml_string_join
