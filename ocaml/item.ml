open Common;;
open Token;;
open Parser;;

let parse_root (ps:pstate) (terminal:token) (*: Ast.root*) = 1;;

  (* let apos = lexpos ps in
   * let parse_lib_name ident =
   *   match peek ps with
   *       EQ ->
   *         begin
   *           bump ps;
   *           match peek ps with
   *               LIT_STR s -> (bump ps; s)
   *             | _ -> raise (unexpected ps)
   *         end
   *     | _ -> ps.pstate_infer_lib_name ident
   * in
   *
   *   match peek ps with *)
