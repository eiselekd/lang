type token =
    PLUS
  | MINUS
  | STAR
  | SLASH

  | IF
  | ELSE
  | DO
  | WHILE
  | ALT
  | CASE

  | LIT_INT       of (int64 * string)

  | IDENT         of string

  | EOF
;;

let rec string_of_tok t =
  match t with
      PLUS       -> "+"
    | MINUS      -> "-"
    | STAR       -> "*"
    | SLASH      -> "/"
    | EOF        -> "<EOF>"

    (* Control-flow keywords *)
    | IF         -> "if"
    | ELSE       -> "else"
    | DO         -> "do"
    | WHILE      -> "while"
    | ALT        -> "alt"
    | CASE       -> "case"

    | LIT_INT (_,s)  -> s

    | IDENT s    -> s
;;
