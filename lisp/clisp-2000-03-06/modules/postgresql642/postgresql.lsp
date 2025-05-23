;; Foreign functions provided by PostgreSQL
;; File: <postgresql.lsp - 1999-03-20 Sat 21:02:41 EST sds@eho.eaglets.com>

(defpackage "SQL"
  (:case-sensitive t)
  (:nicknames "POSTGRES" "POSTGRESQL")
  (:use))

;; This requires linking with NEW_LIBS='postgresql.o -lpq'.

(lisp:in-package "LISP")

(eval-when (compile eval)
  ;; A temporary package, case-insensitive, so that we don't need to prefix
  ;; everything with "lisp:" or "ffi:".
  (defpackage "SQL-AUX" (:use "LISP" "FFI"))
  (in-package "SQL-AUX")
  ;; Symbols to be substituted
  (defconstant substitution
    '((sql::compile . lisp:compile)
      (sql::eval . lisp:eval)
      (sql::gensym . lisp:gensym)
      (sql::let . lisp:let)
      (sql::load . lisp:load)
      (sql::load-time-value . lisp:load-time-value)
      (sql::progn . lisp:progn)
      (sql::setf . lisp:setf)
      (sql::t . lisp:t)
      (sql::bitsizeof . ffi:bitsizeof)
      (sql::boolean . ffi:boolean)
      (sql::char . ffi:char)
      (sql::character . ffi:character)
      (sql::c-array . ffi:c-array)
      (sql::c-array-max . ffi:c-array-max)
      (sql::c-array-ptr . ffi:c-array-ptr)
      (sql::c-function . ffi:c-function)
      (sql::c-ptr . ffi:c-ptr)
      (sql::c-pointer . ffi:c-pointer)
      (sql::c-string . ffi:c-string)
      (sql::c-struct . ffi:c-struct)
      (sql::deref . ffi:deref)
      (sql::double-float . ffi:double-float)
      (sql::element . ffi:element)
      (sql::int . ffi:int)
      (sql::long . ffi:long)
      (sql::nil . ffi:nil)
      (sql::short . ffi:short)
      (sql::sint8 . ffi:sint8)
      (sql::sint16 . ffi:sint16)
      (sql::sint32 . ffi:sint32)
      (sql::sint64 . ffi:sint64)
      (sql::single-float . ffi:single-float)
      (sql::sizeof . ffi:sizeof)
      (sql::slot . ffi:slot)
      (sql::uchar . ffi:uchar)
      (sql::uint . ffi:uint)
      (sql::uint8 . ffi:uint8)
      (sql::uint16 . ffi:uint16)
      (sql::uint32 . ffi:uint32)
      (sql::uint64 . ffi:uint64)
      (sql::ulong . ffi:ulong)
      (sql::ushort . ffi:ushort)))

  ;; We want to export all the symbols defined in this file.
  (macrolet ((exporting (defining-macro-name)
               (let ((original-macro-name (intern (string-upcase
                                                   defining-macro-name) "FFI"))
                     (new-macro-name (intern defining-macro-name "SQL")))
                 `(progn
                   (defmacro ,new-macro-name (name &rest more)
                     `(progn
                       (export ',name)
                       (,',original-macro-name ,name ,@(sublis substitution
                                                               more)))))))
             (normal (defining-macro-name)
               (let ((original-macro-name (intern (string-upcase
                                                   defining-macro-name) "FFI"))
                     (new-macro-name (intern defining-macro-name "SQL")))
                 `(progn
                   (defmacro ,new-macro-name (&rest more)
                     `(,',original-macro-name ,@(sublis substitution
                                                        more)))))))
    (exporting "defconstant")
    (exporting "defun")
    (exporting "defmacro")
    (exporting "define-modify-macro")
    (exporting "define-symbol-macro")
    (exporting "def-c-type")
    (exporting "def-c-enum")
    (exporting "def-c-struct")
    (exporting "def-c-var")
    (exporting "def-c-call-out")
    (normal "c-lines")
    (normal "eval-when")))

(in-package "SQL")

;;; The include files are found in /usr/include/pgsql/
;;;                             or /usr/lib/pgsql/include/

;;; ================= <libpq-fe.h> =================

;;; ----------------- <postgres_ext.h> -----------------

(def-c-type Oid uint)

(eval-when (load compile eval)
  (defconstant NAMEDATALEN 32)
  (defconstant OIDNAMELEN 36))

;;; ----------------- <libpq/pqcomm.h> -----------------
;;; contains only uninteresting low-level stuff

;;; ----------------- <pgsql/libpq-fe.h> -----------------

(def-c-enum ConnStatusType CONNECTION_OK CONNECTION_BAD)

(def-c-enum ExecStatusType
    PGRES_EMPTY_QUERY PGRES_COMMAND_OK PGRES_TUPLES_OK PGRES_COPY_OUT
    PGRES_COPY_IN PGRES_BAD_RESPONSE PGRES_NONFATAL_ERROR PGRES_FATAL_ERROR)

(def-c-var pgresStatus (:type (c-array-ptr c-string)) (:read-only t))

;;(def-c-type PGconn (c-struct vector)) ; components unknown
;;(def-c-type PGresult (c-struct vector)) ; components unknown
;;(def-c-struct PGconn) ; components unknown
;;(def-c-struct PGresult) ; components unknown
(def-c-type PGconn c-pointer) ; components unknown
(def-c-type PGresult c-pointer) ; components unknown

#|
(def-c-struct PGresAttDesc
  (name c-string)
  (adtid Oid)
  (adtsize short))
(def-c-struct PGresAttValue
  (len int)
  (value c-pointer))
|#

(def-c-struct PGnotify
  (result (c-array character #.NAMEDATALEN))
  (be_pid int))

(def-c-type PQnoticeProcessor
    (c-function (:arguments (p1 c-pointer) (message c-string))
                (:return-type c-pointer)))

(def-c-type pqbool char)

(def-c-struct PQprintOpt
  (header pqbool)
  (align pqbool)
  (standard pqbool)
  (html3 pqbool)
  (expanded pqbool)
  (pager pqbool)
  (fieldSep c-string)
  (tableOpt c-string)
  (caption c-string)
  (fieldName (c-array-ptr c-string)))

(def-c-struct PQconninfoOption
  (keyword c-string)
  (envvar c-string)
  (compiled c-string)
  (val c-string)
  (label c-string)
  (dispchar c-string)
  (dispsize int))

(def-c-struct PQArgBlock
  (len int)
  (isint int))
  ;(u (c-union (ptr (c-ptr int))
  ;            (integer int))))

(def-c-call-out PQconnectdb
    (:arguments (conninfo c-string)) (:return-type PGconn))
(def-c-call-out PQsetdbLogin
    (:arguments (pghost c-string) (pgport c-string) (pgoptions c-string)
                (pgtty c-string) (dbname c-string) (login c-string)
                (pwd c-string))
  (:return-type PGconn))
(defmacro PQsetdb (a0 a1 a2 a3 a4) `(PQsetdbLogin ,a0 ,a1 ,a2 ,a3 ,a4 nil nil))
(def-c-call-out PQconndefaults
    (:arguments) (:return-type (c-ptr PQconninfoOption)))
(def-c-call-out PQfinish
    (:arguments (conn PGconn)) (:return-type nil))
(def-c-call-out PQreset
    (:arguments (conn PGconn)) (:return-type nil))
(def-c-call-out PQrequestCancel
    (:arguments (conn PGconn)) (:return-type int))
(def-c-call-out PQdb
    (:arguments (conn PGconn)) (:return-type c-string))
(def-c-call-out PQuser
    (:arguments (conn PGconn)) (:return-type c-string))
(def-c-call-out PQpass
    (:arguments (conn PGconn)) (:return-type c-string))
(def-c-call-out PQhost
    (:arguments (conn PGconn)) (:return-type c-string))
(def-c-call-out PQport
    (:arguments (conn PGconn)) (:return-type c-string))
(def-c-call-out PQtty
    (:arguments (conn PGconn)) (:return-type c-string))
(def-c-call-out PQoptions
    (:arguments (conn PGconn)) (:return-type c-string))
(def-c-call-out PQstatus
    (:arguments (conn PGconn)) (:return-type int)) ; ConnStatusType
(def-c-call-out PQerrorMessage
    (:arguments (conn PGconn)) (:return-type c-string))
(def-c-call-out PQsocket
    (:arguments (conn PGconn)) (:return-type int))
(def-c-call-out PQbackendPID
    (:arguments (conn PGconn)) (:return-type int))
(def-c-call-out PQtrace
    (:arguments (conn PGconn) (debug_port c-pointer)) ; ?? FILE*
  (:return-type nil))
(def-c-call-out PQuntrace
    (:arguments (conn PGconn)) (:return-type nil))
(def-c-call-out PQsetNoticeProcessor ; not in Pg.pm
    (:arguments (conn PGconn) (proc PQnoticeProcessor)
                (arg c-pointer))
  (:return-type nil))
(def-c-call-out PQexec
    (:arguments (conn PGconn) (query c-string))
  (:return-type PGresult))
(def-c-call-out PQnotifies
    (:arguments (conn PGconn))
  (:return-type c-pointer :malloc-free)) ; ?? (c-ptr PGnotify)
(def-c-call-out PQsendQuery
    (:arguments (conn PGconn) (query c-string))
  (:return-type int))
(def-c-call-out PQgetResult
    (:arguments (conn PGconn))
  (:return-type PGresult))
(def-c-call-out PQisBusy
    (:arguments (conn PGconn)) (:return-type int))
(def-c-call-out PQconsumeInput
    (:arguments (conn PGconn)) (:return-type int))
(def-c-call-out PQgetline
    (:arguments (conn PGconn) (string c-string) (length int))
  (:return-type int))
(def-c-call-out PQputline
    (:arguments (conn PGconn) (string c-string))
  (:return-type int))
(def-c-call-out PQgetlineAsync
    (:arguments (conn PGconn) (buffer c-string) (bufsize int))
  (:return-type int))
(def-c-call-out PQputnbytes
    (:arguments (conn PGconn) (buffer c-string) (nbytes int))
  (:return-type int))
(def-c-call-out PQendcopy
    (:arguments (conn PGconn)) (:return-type int))
(def-c-call-out PQfn            ; [security hole] not in Pg.pm
    (:arguments (conn PGconn) (fnid int) (result_buf (c-ptr int))
                (result_len (c-ptr int)) (result_is_int int)
                (args (c-array-ptr PQArgBlock)) (nargs int))
  (:return-type PGresult))
(def-c-call-out PQmakeEmptyPGresult
    (:arguments (conn PGconn) (status int)) ; ?? ExecStatusType
  (:return-type PGresult))
(def-c-call-out PQresultStatus
    (:arguments (res PGresult)) (:return-type int)) ; ?? ExecStatusType
(def-c-call-out PQresultErrorMessage ; not in Pg.pm
    (:arguments (res PGresult)) (:return-type c-string))
(def-c-call-out PQntuples
    (:arguments (res PGresult)) (:return-type int))
(def-c-call-out PQnfields
    (:arguments (res PGresult)) (:return-type int))
(def-c-call-out PQbinaryTuples
    (:arguments (res PGresult)) (:return-type int))
(def-c-call-out PQfname
    (:arguments (res PGresult) (field_num int))
  (:return-type c-string))
(def-c-call-out PQfnumber
    (:arguments (res PGresult) (field_name c-string))
  (:return-type int))
(def-c-call-out PQftype
    (:arguments (res PGresult) (field_num int)) (:return-type int))
(def-c-call-out PQfsize
    (:arguments (res PGresult) (field_num int)) (:return-type int))
(def-c-call-out PQfmod
    (:arguments (res PGresult) (field_num int)) (:return-type int))
(def-c-call-out PQcmdStatus
    (:arguments (res PGresult)) (:return-type c-string))
(def-c-call-out PQoidStatus
    (:arguments (res PGresult)) (:return-type c-string))
(def-c-call-out PQcmdTuples
    (:arguments (res PGresult)) (:return-type c-string))
(def-c-call-out PQgetvalue
    (:arguments (res PGresult) (tup_num int) (field_num int))
  (:return-type c-string))
(def-c-call-out PQgetlength
    (:arguments (res PGresult) (tup_num int) (field_num int))
  (:return-type int))
(def-c-call-out PQgetisnull
    (:arguments (res PGresult) (tup_num int) (field_num int))
  (:return-type int))
(def-c-call-out PQclear
    (:arguments (res PGresult)) (:return-type nil))
(def-c-call-out PQprint
    (:arguments (fout c-pointer) ; ?? FILE*
                (res PGresult)
                (ps (c-ptr PQprintOpt)))
  (:return-type nil))
(def-c-call-out PQdisplayTuples
    (:arguments (res PGresult) (fp c-pointer) ; ?? FILE*
                (fillAlign int)
                (fieldSep c-string) (printHeader int) (quiet int))
  (:return-type nil))
(def-c-call-out PQprintTuples
    (:arguments (res PGresult) (fout c-pointer) ; ?? FILE*
                (printAttName int) (terseOutput int) (width int))
  (:return-type nil))
(def-c-call-out PQmblen         ; not in Pg.pm
    (:arguments (s (c-ptr uchar))) (:return-type int))
(def-c-call-out lo_open
    (:arguments (conn PGconn) (lobjId Oid) (mode int))
  (:return-type int))
(def-c-call-out lo_close
    (:arguments (conn PGconn) (fd int)) (:return-type int))
(def-c-call-out lo_read
    (:arguments (conn PGconn) (fd int) (buf c-string) (len int))
  (:return-type int))
(def-c-call-out lo_write
    (:arguments (conn PGconn) (fd int) (buf c-string) (len int))
  (:return-type int))
(def-c-call-out lo_lseek
    (:arguments (conn PGconn) (fd int) (offset int) (whence int))
  (:return-type int))
(def-c-call-out lo_creat
    (:arguments (conn PGconn) (mode int)) (:return-type Oid))
(def-c-call-out lo_tell
    (:arguments (conn PGconn) (fd int)) (:return-type int))
(def-c-call-out lo_unlink
    (:arguments (conn PGconn) (lobjId Oid)) (:return-type int))
(def-c-call-out lo_import
    (:arguments (conn PGconn) (filename c-string))
  (:return-type Oid))
(def-c-call-out lo_export
    (:arguments (conn PGconn) (lobjId Oid) (filename c-string))
  (:return-type int))

;; not found:
;; PGRES_INV_SMGRMASK
;; PGRES_INV_ARCHIVE
;; PGRES_INV_WRITE
;; PGRES_INV_READ
;; PGRES_InvalidOid

(lisp:in-package "LISP")

(eval-when (compile eval) (delete-package "SQL-AUX"))

