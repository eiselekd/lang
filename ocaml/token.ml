type token =
    PLUS
  | MINUS
  | STAR
  | SLASH

  | EOF
;;

let rec string_of_tok t =
  match t with
      PLUS       -> "+"
    | MINUS      -> "-"
    | STAR       -> "*"
    | SLASH      -> "/"
    | EOF        -> "<EOF>"
;;
