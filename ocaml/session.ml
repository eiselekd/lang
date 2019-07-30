open Common;;

type sess =
{
  mutable sess_in: filename option;
  mutable sess_out: filename option;
  mutable sess_log_lex: bool;
  mutable sess_log_parse: bool;
  mutable sess_log_ast: bool;
  mutable sess_log_out: out_channel;
}
;;

let log name flag chan =
  let k1 s =
    Printf.fprintf chan "%s: %s\n%!" name s
  in
  let k2 _ = () in
    Printf.ksprintf (if flag then k1 else k2)
;;

let filename_of (fo:filename option) : filename =
  match fo with
      None -> "<none>"
    | Some f -> f
;;

