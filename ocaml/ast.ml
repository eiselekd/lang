open Common;;

type ident = string
;;

type root =
    BASE_app of expr

and lval =
    LVAL_base of atom

and lit =
  | LIT_nil
  | LIT_bool of bool
  | LIT_int of (int64 * string)
  | LIT_uint of (int64 * string)
  | LIT_char of int

and atom =
    ATOM_literal of lit
  | ATOM_lval of lval

and expr =
    EXPR_binary of (binop * expr * expr)
  | EXPR_atom of atom

and binop =
    BINOP_add
  | BINOP_sub

;;

