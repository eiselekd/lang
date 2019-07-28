open Token;;
open Lex;;

let add a = a + 1;;

let introtext = Printf.fprintf stdout "Lex '%s'" Sys.argv.(1);;

let nexttok lexbuf =
  let tok = Lex.token lexbuf in
  if tok = EOF
  then false
  else
    (Printf.fprintf stdout "'%s'" (Token.string_of_tok tok); true)
;;

let doscan fn =
  let fname = fn in
  let lexbuf = Lexing.from_channel (open_in fname) in
  let spos = { lexbuf.Lexing.lex_start_p with Lexing.pos_fname = fname } in
  let cpos = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname } in
  lexbuf.Lexing.lex_start_p <- spos;
  lexbuf.Lexing.lex_curr_p <- cpos;
  while (nexttok lexbuf) do
    Printf.fprintf stdout ".";
  done
;;

let _ = (introtext;
         doscan Sys.argv.(1) )
;;

(*
 * Local Variables:
 * fill-column: 78;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C . 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
