open Common;;
open Token;;
open Parser;;




let rec parse_root (ps:pstate) (terminal:token) : Ast.root =
  match Parser.peek ps with
    LIT_INT (i,s) -> Ast.BASE_app (parse_term_expr ps)
   | _ -> raise (unexpected ps)

and binop_rhs
    (ps:pstate)
    (name:string)
    (apos:pos)
    (lhs:Ast.expr)
    (rhs_parse_fn:pstate -> Ast.expr)
    (op:Ast.binop)
    : Ast.expr =
  bump ps;
  let rhs = (ctxt (name ^ " rhs") rhs_parse_fn ps) in
  let bpos = lexpos ps in
    Ast.EXPR_binary (op, lhs, rhs)

and parse_term_expr (ps:pstate) : Ast.expr =
  let name = "term expr" in
  let apos = lexpos ps in
  let lhs = ctxt (name ^ " lhs") parse_lit ps in
    match peek ps with
        PLUS  -> binop_rhs ps name apos lhs parse_lit Ast.BINOP_add
      | MINUS -> binop_rhs ps name apos lhs parse_lit Ast.BINOP_sub
      | _     -> lhs

and parse_lit (ps:pstate) : Ast.expr =
  match peek ps with
      LIT_INT (n,s) -> (bump ps; (Ast.EXPR_atom (Ast.ATOM_literal (Ast.LIT_int(n,s)))))
    | _ -> raise (unexpected ps)
;;


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
