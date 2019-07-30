type ast =
   STMT of (stmt array)

and stmt =
  STMT_EXPR of exp
| STMT_IF of (exp * stmt * stmt)

and exp =
  PLUS of (lit * lit)
| MINUS of (lit * lit)

and lit =
    LIT_nil
  | LIT_int of (int * string)
  | LIT_bool of bool
;;

(* ----------------------------- *)

let fmt = Format.fprintf;;

let rec fmt_stmt (ff:Format.formatter) (s: ast) : unit =
  fmt ff "()"

and fmt_lit (ff:Format.formatter) (l:lit) : unit =
  match l with
  | LIT_nil -> fmt ff "()"
  | LIT_bool true -> fmt ff "true"
  | LIT_bool false -> fmt ff "false"
  | LIT_int (i,s) -> fmt ff "%d(%s)" i s
;;

let fmt_to_str (f:Format.formatter -> 'a -> unit) (v:'a) : string =
  let buf = Buffer.create 16 in
  let bf = Format.formatter_of_buffer buf in
    begin
      f bf v;
      Format.pp_print_flush bf ();
      Buffer.contents buf
    end

let test_out () =
  Printf.printf "%s" (fmt_to_str fmt_lit (LIT_int (1,"1")))
;;

test_out ();;
