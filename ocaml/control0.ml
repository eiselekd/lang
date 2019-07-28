(* https://ocaml.org/learn/tutorials/if_statements_loops_and_recursion.html *)

open Fmt;;

let main () =
  1;;
(*  Printf.fprintf stdout "Control0\n";;
*)

let ifelse a =
  (
    if a
    then Printf.printf  "True\n"
    else Printf.printf  "False\n"
  )
;;

let rec r a b =
  if a > b then []
  else a :: r (a+1) b
;;

main ()
;;
ifelse true ;;
ifelse false
;;

let test_list l =
  Format.printf "%a\n" (Fmt.Dump.list Fmt.int) l
;;

test_list (r 1 10)
;;

let f elem =
  Format.printf "I'm looking at element %s now\n" elem;;

List.iter f ["a"; "b"; ]
;;
