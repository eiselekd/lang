
let _ =
  (Printf.fprintf stderr "Test\n"; exit 1)
;;

(*
 * Local Variables:
 * fill-column: 78;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C . 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
