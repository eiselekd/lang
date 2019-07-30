open Common;;
open Token;;
open Parser;;
open Item;;

let parse_src_file
    (sess:Session.sess)
    (*: Ast.root*) =
  let fname = Session.filename_of sess.sess_in in
  let ps = make_parser sess fname in
  with_err_handling sess
      begin
        fun _ ->
          let apos = lexpos ps in
          let items = Item.parse_root ps EOF in
          let bpos = lexpos ps in
          let files = Hashtbl.create 0 in
          1
      end

;;
