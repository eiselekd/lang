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
      ps
;;

let parse_src_file
    (sess:Session.sess)
    : Ast.root =
  let fname = Session.filename_of sess.sess_in in
  let ps = make_parser sess fname in

  Ast.BASE_app [||]
;;
