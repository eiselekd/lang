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

and atom =
    ATOM_literal of lit

and expr =
    EXPR_binary of (binop * expr * expr)
  | EXPR_atom of atom

and binop =
    BINOP_add
  | BINOP_sub

;;

(* -------------------------------------- *)

let fmt = Format.fprintf;;

let rec fmt_root (ff:Format.formatter) (r: root) : unit =
  match r with
  | BASE_app e -> fmt_expr ff e;

and fmt_lit (ff:Format.formatter) (l:lit) : unit =
  match l with
  | LIT_nil -> fmt ff "()"
  | LIT_bool true -> fmt ff "true"
  | LIT_bool false -> fmt ff "false"
  | LIT_int (i,s) -> fmt ff "%s" s

and fmt_binop (ff:Format.formatter) (b:binop) : unit =
  fmt ff "%s"
    begin
      match b with
        | BINOP_add -> "+"
        | BINOP_sub -> "-"
    end

and fmt_atom (ff:Format.formatter) (a:atom) : unit =
  match a with
      ATOM_literal lit -> fmt_lit ff lit

and fmt_expr (ff:Format.formatter) (e:expr) : unit =
  match e with
    EXPR_binary (b,a1,a2) ->
      begin
        fmt_expr ff a1;
        fmt ff " ";
        fmt_binop ff b;
        fmt ff " ";
        fmt_expr ff a2
      end
  | EXPR_atom a -> fmt_atom ff a

;;

let fmt_to_str (f:Format.formatter -> 'a -> unit) (v:'a) : string =
  let buf = Buffer.create 16 in
  let bf = Format.formatter_of_buffer buf in
    begin
      f bf v;
      Format.pp_print_flush bf ();
      Buffer.contents buf
    end
;;
