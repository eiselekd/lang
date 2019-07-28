{
  open Token;;
  (* open Common;; *)

  let bump_line p = { p with
              Lexing.pos_lnum = p.Lexing.pos_lnum + 1;
              Lexing.pos_bol = p.Lexing.pos_cnum }
  ;;

  exception Lex_err of (string * Common.pos);;


  let keyword_table = Hashtbl.create 100
  let _ =
    List.iter (fun (kwd, tok) -> Common.htab_put keyword_table kwd tok)
              [ ("if", IF);
                ("else", ELSE);
                ("while", WHILE);
                ("do", DO);
                ("alt", ALT);
                ("case", CASE);
              ]
  ;;

}

let hexdig = ['0'-'9' 'a'-'f' 'A'-'F']
let bin = "0b" ['0' '1']['0' '1' '_']*
let hex = "0x" hexdig ['0'-'9' 'a'-'f' 'A'-'F' '_']*
let dec = ['0'-'9']+
let exp = ['e''E']['-''+']? dec
let flo = (dec '.' dec (exp?)) | (dec exp)

let ws = [ ' ' '\t' '\r' ]

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  ws+                          { token lexbuf }
| '\n'                         { lexbuf.Lexing.lex_curr_p
                                     <- (bump_line lexbuf.Lexing.lex_curr_p);
                                 token lexbuf }
| "//" [^'\n']*                { token lexbuf }

| '+'                          { PLUS       }
| '-'                          { MINUS      }
| '*'                          { STAR       }
| '/'                          { SLASH      }

| id as i
                               { try
                                     Hashtbl.find keyword_table i
                                 with
                                     Not_found -> IDENT (i)
                               }


| eof                          { EOF        }


(*
 * Local Variables:
 * fill-column: 78;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C . 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
