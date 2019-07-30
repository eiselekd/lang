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

exception Parse_err of (pstate * string)
;;

let with_err_handling sess thunk =
  try
    thunk ()
  with
    Parse_err (ps, str) ->
    1
;;

let lexpos (ps:pstate) : pos =
  let p = ps.pstate_lexbuf.Lexing.lex_start_p in
    (p.Lexing.pos_fname,
     p.Lexing.pos_lnum ,
     (p.Lexing.pos_cnum) - (p.Lexing.pos_bol))
;;

(*  Ast.BASE_app [||]
*)
