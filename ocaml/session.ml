open Common;;

type sess =
{
  mutable sess_in: filename option;
  mutable sess_out: filename option;
}
;;

let filename_of (fo:filename option) : filename =
  match fo with
      None -> "<none>"
    | Some f -> f
;;

