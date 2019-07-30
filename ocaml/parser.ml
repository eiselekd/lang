open Common;;
open Token;;

type pstate =
    { mutable pstate_peek : token;
      mutable pstate_ctxt : (string * pos) list;
      mutable pstate_rstr : bool;
      mutable pstate_depth: int;
      pstate_lexbuf       : Lexing.lexbuf;
      pstate_file         : filename;
      pstate_sess         : Session.sess;
    }
;;


exception Parse_err of (pstate * string)
;;


let iflog ps thunk =
  if ps.pstate_sess.Session.sess_log_parse
  then thunk ()
  else ()
;;

let log (ps:pstate) = Session.log "parse"
  ps.pstate_sess.Session.sess_log_parse
  ps.pstate_sess.Session.sess_log_out
;;

let make_parser
    (sess:Session.sess)
    (fname:string)
    : pstate =
  let lexbuf = Lexing.from_channel (open_in fname) in
  let spos = { lexbuf.Lexing.lex_start_p with Lexing.pos_fname = fname } in
  let cpos = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname } in
    lexbuf.Lexing.lex_start_p <- spos;
    lexbuf.Lexing.lex_curr_p <- cpos;
    let first = Lex.token lexbuf in
    let ps =
      { pstate_peek = first;
        pstate_ctxt = [];
        pstate_rstr = false;
        pstate_depth = 0;
        pstate_lexbuf = lexbuf;
        pstate_file = fname;
        pstate_sess = sess;
 }
    in
      iflog ps (fun _ -> log ps "made parser for: %s\n%!" fname);
      ps
;;


let with_err_handling sess thunk =
  try
    thunk ()
  with
    Parse_err (ps, str) ->
     assert false
(* https://ocaml.org/learn/tutorials/if_statements_loops_and_recursion.html *)
;;

let lexpos (ps:pstate) : pos =
  let p = ps.pstate_lexbuf.Lexing.lex_start_p in
    (p.Lexing.pos_fname,
     p.Lexing.pos_lnum ,
     (p.Lexing.pos_cnum) - (p.Lexing.pos_bol))

;;

let ctxt (n:string) (f:pstate -> 'a) (ps:pstate) : 'a =
  (ps.pstate_ctxt <- (n, lexpos ps) :: ps.pstate_ctxt;
   let res = f ps in
     ps.pstate_ctxt <- List.tl ps.pstate_ctxt;
     res)
;;

(* Bottom-most parser actions. *)

let peek (ps:pstate) : token =
  iflog ps
    begin
      fun _ ->
        log ps "peeking at: %s     // %s"
          (string_of_tok ps.pstate_peek)
          (match ps.pstate_ctxt with
               (s, _) :: _ -> s
             | _ -> "<empty>")
    end;
  ps.pstate_peek
;;


let bump (ps:pstate) : unit =
  begin
    iflog ps (fun _ -> log ps "bumping past: %s"
                (string_of_tok ps.pstate_peek));
    ps.pstate_peek <- Lex.token ps.pstate_lexbuf
  end
;;

let err (str:string) (ps:pstate) =
  (Parse_err (ps, (str)))
;;

let unexpected (ps:pstate) =
  err ("Unexpected token '" ^ (string_of_tok (peek ps)) ^ "'") ps
;;


(*  Ast.BASE_app [||]
*)
