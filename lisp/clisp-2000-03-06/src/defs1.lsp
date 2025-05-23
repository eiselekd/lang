;;;; Einige Definitionen von Standard-Funktionen in LISP
;;;; 1.8.1989, 2.9.1989, 8.10.1989

(in-package "LISP")
(export '(doseq dohash #-(or UNIX WIN32) *default-time-zone*))
(in-package "SYSTEM")


;;; Funktionen f�r Symbole (Kapitel 10)

(defun copy-symbol (symbol &optional flag)
                   ;; Common LISP, S. 169
  (let ((sym (make-symbol (symbol-name symbol))))
    (when flag
      (when (boundp symbol) (sys::set-symbol-value sym (%symbol-value symbol)))
      (when (fboundp symbol) (sys::%putd sym (symbol-function symbol)))
      (sys::%putplist sym (copy-list (symbol-plist symbol)))
    )
    sym
) )

(let ((gentemp-count 0))
  (defun gentemp (&optional (prefix "T") (package *package*))
                 ;; Common LISP, S. 170
    (loop
      (setq gentemp-count (1+ gentemp-count))
      (multiple-value-bind (sym flag)
        (intern
          (string-concat prefix
            (write-to-string gentemp-count :base 10 :radix nil :readably nil)
          )
          package
        )
        (unless flag (return sym))
) ) ) )


;;; Macros f�r Packages (Kapitel 11), S. 187-188

(defmacro do-symbols ((var &optional (packageform '*package*) (resultform nil))
                      &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    (let ((packvar (gensym)))
      `(BLOCK NIL
         (LET ((,packvar ,packageform))
           (LET ((,var NIL))
             (DECLARE (IGNORABLE ,var) ,@declarations)
             (SYSTEM::MAP-SYMBOLS
               #'(LAMBDA (,var)
                   ,@(if declarations `((DECLARE ,@declarations)) '())
                   ,@body-rest
                 )
               ,packvar
             )
             ,resultform
       ) ) )
) ) )

(defmacro do-external-symbols ((var &optional (packageform '*package*) (resultform nil))
                               &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    (let ((packvar (gensym)))
      `(BLOCK NIL
         (LET ((,packvar ,packageform))
           (LET ((,var NIL))
             (DECLARE (IGNORABLE ,var) ,@declarations)
             (SYSTEM::MAP-EXTERNAL-SYMBOLS
               #'(LAMBDA (,var)
                   ,@(if declarations `((DECLARE ,@declarations)) '())
                   ,@body-rest
                 )
               ,packvar
             )
             ,resultform
       ) ) )
) ) )

(defmacro do-all-symbols ((var &optional (resultform nil))
                          &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    `(BLOCK NIL
       (LET ((,var NIL))
         (DECLARE (IGNORABLE ,var) ,@declarations)
         (SYSTEM::MAP-ALL-SYMBOLS
           #'(LAMBDA (,var)
               ,@(if declarations `((DECLARE ,@declarations)) '())
               ,@body-rest
             )
         )
         ,resultform
     ) )
) )

;;; <HS>/Body/mac_with-package-iterator.html
(defmacro with-package-iterator ((name pack-list &rest types) &body body)
  (unless types
    (error-of-type 'source-program-error
      (ENGLISH "missing symbol types (~S/~S/~S) in ~S")
      ':internal ':external ':inherited 'with-package-iterator
  ) )
  (dolist (symboltype types)
    (case symboltype
      ((:INTERNAL :EXTERNAL :INHERITED))
      (t (error-of-type 'source-program-error
           (ENGLISH "~S: flag must be one of the symbols ~S, ~S, ~S, not ~S")
           'with-package-iterator ':internal ':external ':inherited symboltype
  ) ) )  )
  (let ((iterfun (gensym "WPI")))
    `(let ((,iterfun (package-iterator-function ,pack-list ',(remove-duplicates types))))
       (macrolet ((,name () '(funcall ,iterfun)))
         ,@body
     ) )
) )
(defun package-iterator-function (pack-list symbol-types)
  (let ((iterstates
          (mapcar #'(lambda (pack) (sys::package-iterator pack symbol-types))
                  (if (listp pack-list) pack-list (list pack-list))
       )) )
    ; The iterstates list is cdr'ed down during the iteration.
    #'(lambda ()
        (loop
          (if iterstates
            (multiple-value-bind (more symb acc)
                (sys::package-iterate (car iterstates))
              (if more
                (return (values more symb acc (svref (car iterstates) 4)))
                (pop iterstates)
            ) )
            (return nil)
      ) ) )
) )

;;; Modulverwaltung (Kapitel 11.8), CLTL S. 188

(defvar *modules* nil)

(defun provide (module-name)
  (setq *modules* (adjoin (string module-name) *modules* :test #'string=))
)

(defun require (module-name &optional (pathname nil p-given))
  (unless (member (string module-name) *modules* :test #'string-equal)
    (unless p-given (setq pathname (pathname module-name)))
    (let (#-CLISP(*default-pathname-defaults* '#""))
      (if (atom pathname) (load pathname) (mapcar #'load pathname))
    )
) )


;;; Konstanten f�r Zahlen (Kapitel 12)

; vgl. File INTLOG.TXT
(defconstant boole-clr 0)
(defconstant boole-set 15)
(defconstant boole-1 10)
(defconstant boole-2 12)
(defconstant boole-c1 5)
(defconstant boole-c2 3)
(defconstant boole-and 8)
(defconstant boole-ior 14)
(defconstant boole-xor 6)
(defconstant boole-eqv 9)
(defconstant boole-nand 7)
(defconstant boole-nor 1)
(defconstant boole-andc1 4)
(defconstant boole-andc2 2)
(defconstant boole-orc1 13)
(defconstant boole-orc2 11)

; Zum Wiedereinlesen von BYTEs:
(defun make-byte (&key size position) (byte size position))

; X3J13 vote <79>
(defconstant least-positive-normalized-short-float least-positive-short-float)
(defconstant least-negative-normalized-short-float least-negative-short-float)
(defconstant least-positive-normalized-single-float least-positive-single-float)
(defconstant least-negative-normalized-single-float least-negative-single-float)
(defconstant least-positive-normalized-double-float least-positive-double-float)
(defconstant least-negative-normalized-double-float least-negative-double-float)
(proclaim
  '(constant-notinline
    least-positive-normalized-short-float
    least-negative-normalized-short-float
    least-positive-normalized-single-float
    least-negative-normalized-single-float
    least-positive-normalized-double-float
    least-negative-normalized-double-float
)  )


;;; Funktionen f�r Sequences (Kapitel 14)

(defmacro doseq ((var seqform &optional resultform) &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    (let ((seqvar (gensym)))
      `(BLOCK NIL
         (LET ((,seqvar ,seqform))
           (LET ((,var NIL))
             (DECLARE (IGNORABLE ,var) ,@declarations)
             (MAP NIL
                  #'(LAMBDA (,var)
                      ,@(if declarations `((DECLARE ,@declarations)) '())
                      (TAGBODY ,@body-rest)
                    )
                  ,seqvar
             )
             ,resultform
       ) ) )
) ) )


;;; Funktionen f�r Listen (Kapitel 15)

; Hilfsversion von MEMBER, die das :KEY-Argument auch auf item anwendet:
(defun sys::member1 (item list &rest rest &key test test-not key)
  (declare (ignore test test-not))
  (unless key (setq key #'identity))
  (apply #'member (funcall key item) list rest)
)

(defun union (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) list2)
        ((apply #'sys::member1 (car list1) list2 rest)
         (apply #'union (cdr list1) list2 rest))
        (t (cons (car list1) (apply #'union (cdr list1) list2 rest)))
  )
  |# ; iterative
  (let ((list1-filtered '()))
    (dolist (item list1)
      (unless (apply #'sys::member1 item list2 rest)
        (setq list1-filtered (cons item list1-filtered))
    ) )
    (nreconc list1-filtered list2)
  )
)

(defun nunion (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) list2)
        ((apply #'sys::member1 (car list1) list2 rest)
         (apply #'nunion (cdr list1) list2 rest))
        (t (rplacd list1 (apply #'nunion (cdr list1) list2 rest)))
  )
  |# ; iterative
  (let ((first nil) (last nil))
    (do ((l list1 (cdr l)))
        ((endp l))
      (unless (apply #'sys::member1 (car l) list2 rest)
        (if last (rplacd last l) (setq first l))
        (setq last l)
    ) )
    (if last (progn (rplacd last list2) first) list2)
  )
)

(defun intersection (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) nil)
        ((apply #'sys::member1 (car list1) list2 rest)
         (cons (car list1)
               (apply #'intersection (cdr list1) list2 rest)))
        (t (apply #'intersection (cdr list1) list2 rest))
  )
  |# ; iterative
  (let ((list1-filtered '()))
    (dolist (item list1)
      (when (apply #'sys::member1 item list2 rest)
        (setq list1-filtered (cons item list1-filtered))
    ) )
    (list-nreverse list1-filtered)
  )
)

(defun nintersection (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) nil)
        ((apply #'sys::member1 (car list1) list2 rest)
         (rplacd list1 (apply #'nintersection (cdr list1) list2 rest)) )
        (t (apply #'nintersection (cdr list1) list2 rest))
  )
  |# ; iterative
  (let ((first nil) (last nil))
    (do ((l list1 (cdr l)))
        ((endp l))
      (when (apply #'sys::member1 (car l) list2 rest)
        (if last (rplacd last l) (setq first l))
        (setq last l)
    ) )
    (if last (progn (rplacd last nil) first) nil)
  )
)

(defun set-difference (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) nil)
        ((not (apply #'sys::member1 (car list1) list2 rest))
         (cons (car list1)
               (apply #'set-difference (cdr list1) list2 rest)
        ))
        (t (apply #'set-difference (cdr list1) list2 rest))
  )
  |# ; iterative
  (let ((list1-filtered '()))
    (dolist (item list1)
      (unless (apply #'sys::member1 item list2 rest)
        (setq list1-filtered (cons item list1-filtered))
    ) )
    (list-nreverse list1-filtered)
  )
)

(defun nset-difference (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  #| ; recursive (not suitable for long lists):
  (cond ((endp list1) nil)
        ((not (apply #'sys::member1 (car list1) list2 rest))
         (rplacd list1 (apply #'nset-difference (cdr list1) list2 rest)) )
        (t (apply #'nset-difference (cdr list1) list2 rest))
  )
  |# ; iterative
  (let ((first nil) (last nil))
    (do ((l list1 (cdr l)))
        ((endp l))
      (unless (apply #'sys::member1 (car l) list2 rest)
        (if last (rplacd last l) (setq first l))
        (setq last l)
    ) )
    (if last (progn (rplacd last nil) first) nil)
  )
)

(defun set-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (append (apply #'set-difference list1 list2 rest)
          (apply #'set-difference list2 list1 rest)
) )

(defun nset-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (nconc (apply #'set-difference list1 list2 rest)
         (apply #'nset-difference list2 list1 rest)
) )

(defun subsetp (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (do ((l list1 (cdr l)))
      ((endp l) t)
    (if (not (apply #'sys::member1 (car l) list2 rest)) (return nil))
) )

; Wie SUBST-IF, nur dass das Ersatz-Element durch eine Funktion gegeben wird
; und nicht konstant sein muss.
(defun subst-if-then (newfun testfun tree &key (key #'identity))
  (labels ((subst (tree)
             (if (funcall testfun (funcall key tree))
               (funcall newfun tree)
               (if (consp tree)
                 (let* ((car (car tree)) (cdr (cdr tree))
                        (newcar (subst car)) (newcdr (subst cdr)))
                   (if (and (eq car newcar) (eq cdr newcdr))
                     tree
                     (cons newcar newcdr)
                 ) )
                 tree
          )) ) )
    (subst tree)
) )


;;; Funktionen f�r Hash-Tabellen (Kapitel 16)

(defmacro dohash ((keyvar valuevar HTform &optional resultform) &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    (let ((HTvar (gensym)))
      `(BLOCK NIL
         (LET ((,HTvar ,HTform))
           (LET ((,keyvar NIL) (,valuevar NIL))
             (DECLARE (IGNORABLE ,keyvar ,valuevar) ,@declarations)
             (MAPHASH
               #'(LAMBDA (,keyvar ,valuevar)
                   ,@(if declarations `((DECLARE ,@declarations)) '())
                   (TAGBODY ,@body-rest)
                 )
               ,HTvar
             )
             ,resultform
       ) ) )
) ) )


;;; Funktionen f�r Strings (Kapitel 18)

(defun string-trim (character-bag string)
  (sys::string-both-trim character-bag character-bag string)
)

(defun string-left-trim (character-bag string)
  (sys::string-both-trim character-bag nil string)
)

(defun string-right-trim (character-bag string)
  (sys::string-both-trim nil character-bag string)
)


;;; Funktionen f�r Pathnames (Kapitel 23.1.5)
#+LOGICAL-PATHNAMES
(progn
  (defun logical-pathname-translations (host)
    (setq host (string-upcase host))
    (or (gethash host sys::*logical-pathname-translations*) ; :test #'equal !
        (error (ENGLISH "~S: ~S does not name a logical host")
               'logical-pathname-translations host
  ) )   )
  (defun set-logical-pathname-translations (host translations)
    (setq host (string-upcase host))
    (puthash host sys::*logical-pathname-translations* ; :test #'equal !
             (let ;((host-pathname (logical-pathname (string-concat host ":;"))))
                  ((host-pathname (sys::make-logical-pathname :host host)))
               (mapcar #'(lambda (rule)
                           (cons (merge-pathnames (logical-pathname (first rule))
                                                  host-pathname 'NIL
                                 )
                                 (rest rule)
                         ) )
                       translations
  ) )        ) )
  (defun load-logical-pathname-translations (host)
    (setq host (string-upcase host))
    (unless (gethash host sys::*logical-pathname-translations*) ; :test #'equal !
      (set-logical-pathname-translations host
        ; Woher bekommt man die Liste der Umsetzungen??
        #| ; Marcus versucht es so:
           (read
             (open
               (merge-pathnames
                 (merge-pathnames host
                   (string-concat (sys::getenv "CLISP_LOGICAL_PATHNAME_TRANSLATIONS"))
                 )
                 (make-pathname :type '#".lsp")
           ) ) )
        |#
        (if (and (fboundp 'sys::getenv)
                 (sys::getenv (string-concat "LOGICAL_HOST_" host "_FROM"))
                 (sys::getenv (string-concat "LOGICAL_HOST_" host "_TO"))
            )
          (list (list (sys::getenv (string-concat "LOGICAL_HOST_" host "_FROM"))
                      (sys::getenv (string-concat "LOGICAL_HOST_" host "_TO"))
          )     )
          (error (ENGLISH "No translations for logical host ~S found")
                 host
  ) ) ) ) )
  (set-logical-pathname-translations "SYS"
    '((";*.LISP" #+DOS "*.LSP" #-DOS "*.lsp") ("*.*" "*.*") ("*" "*"))
  )
)


;;; Funktionen f�r Zeit (Kapitel 25.4.1)

; Hilfsfunktion f�r Macro TIME
(defun %time (new-real1 new-real2 new-run1 new-run2 new-gc1 new-gc2
              new-space1 new-space2 new-gccount
              old-real1 old-real2 old-run1 old-run2 old-gc1 old-gc2
              old-space1 old-space2 old-gccount)
  (macrolet ((merge-2-values (val1 val2)
               (if (< internal-time-units-per-second 1000000)
                 `(dpb ,val1 (byte 16 16) ,val2) ; TIME_1: AMIGA, DOS, OS/2, UNIX_TIMES
                 `(+ (* ,val1 internal-time-units-per-second) ,val2) ; TIME_2: UNIX sonst, WIN32
            )) )
    (let ((Real-Time (- (merge-2-values new-real1 new-real2)
                        (merge-2-values old-real1 old-real2)
          )          )
          (Run-Time (- (merge-2-values new-run1 new-run2)
                       (merge-2-values old-run1 old-run2)
          )         )
          (GC-Time (- (merge-2-values new-gc1 new-gc2)
                      (merge-2-values old-gc1 old-gc2)
          )        )
          (Space (- (dpb new-space1 (byte 24 24) new-space2)
                    (dpb old-space1 (byte 24 24) old-space2)
          )      )
          (GC-Count (- new-gccount old-gccount))
          (stream *trace-output*)
         )
      (terpri stream)
      (write-string "Real time: " stream)
      (write (float (/ Real-Time internal-time-units-per-second)) :stream stream)
      (write-string " sec." stream)
      (terpri stream)
      (write-string "Run time: " stream)
      (write (float (/ Run-Time internal-time-units-per-second)) :stream stream)
      (write-string " sec." stream)
      (terpri stream)
      (write-string "Space: " stream)
      (write Space :stream stream)
      (write-string " Bytes" stream)
      (when (or (plusp GC-Count) (plusp GC-Time))
        (terpri stream)
        (write-string "GC: " stream) (write GC-Count :stream stream)
        (write-string ", GC time: " stream)
        (write (float (/ GC-Time internal-time-units-per-second)) :stream stream)
        (write-string " sec." stream)
      )
) ) )

; (sleep seconds) macht seconds Sekunden Pause. CLTL S. 447
(defun sleep (time)
  (if (and (realp time) (not (minusp time)))
    (progn
      ; Diese Fallunterscheidung h�ngt von sys::%sleep in time.d ab.
      #+(or AMIGA DOS OS/2 ACORN-RISCOS) ; SLEEP_1
      (if (> time '#,(floor (expt 2 31) internal-time-units-per-second))
        ; Mehr als 248 bzw. 994 bzw. 497 Tage? (Denn sys::%sleep akzeptiert nur
        ; Argumente < 2^32, bei #+(or DOS OS/2) sogar nur Argumente < 2^31.)
        (loop ; ja -> Endlosschleife
          (sys::%sleep '#,(* 86400 internal-time-units-per-second))
        )
        (sys::%sleep (round (* time internal-time-units-per-second)))
      )
      #+UNIX ; SLEEP_2
      (if (> time 16700000) ; mehr als 193 Tage?
        (loop (sys::%sleep 86400 0)) ; ja -> Endlosschleife
        (multiple-value-bind (seconds rest) (floor time)
          (sys::%sleep seconds (round (* rest 1000000)))
      ) )
      #+WIN32 ; SLEEP_2
      (if (> time 4250000) ; mehr als 49 Tage?
        (loop (sys::%sleep 86400 0)) ; ja -> Endlosschleife
        (multiple-value-bind (seconds rest) (floor time)
          (sys::%sleep seconds (round (* rest 1000)))
      ) )
    )
    (error-of-type 'type-error
      :datum time :expected-type '(REAL 0 *)
      (ENGLISH "~S: argument ~S should be a nonnegative number")
      'sleep time
) ) )


;; Funktionen f�r Zeit-Umrechnung und Zeitzonen (CLTL Kapitel 25.4.1)
;; Version 2, beinhaltet mehr Mathematik und basiert auf M�rz-Jahren

; Ein M�rz-Jahr sei die Periode vom 1.3. bis 28/29.2.
; Vorteil: Umrechnung Monat/Tag <--> Jahrtag wird einfacher.
; Skizze:
;   1.1.1900            1.1.1901            1.1.1902
;                                         
;   |-------------------|-------------------|-------------------|
;   |     Jahr 1900     |     Jahr 1901     |     Jahr 1902     |
;   |--|----------------|--|----------------|--|----------------|--|
;      |  M�rz-Jahr 1900   |  M�rz-Jahr 1901   |  M�rz-Jahr 1902   |
;      |-------------------|-------------------|-------------------|
;                                            
;      1.3.1900            1.3.1901            1.3.1902

; (UTag Jahr) = Nummer des Tages 1.3.Jahr (gegen�ber 1.1.1900)
; UTag(J) = 365*J + floor(J/4) - floor(J/100) + floor(J/400) - 693901
; damit  UTag(J) - UTag(J-1) = 365 + [1 falls J Schaltjahr]
; und    UTag(1899) = -306
; gelten.
(defun UTag (Jahr)
  (+ (* 365 Jahr) (floor Jahr 4) (- (floor Jahr 100)) (floor Jahr 400) -693901)
)

; N�herungwert:
; 365+1/4-1/100+1/400 = 365.2425 = 146097/400 .
; Durch Betrachtung einer Wertetabelle der 400-periodischen Funktion
; (J -> UTag(J)-146097/400*J) sieht man:
;   146097/400*J - 693902.4775 <= UTag(J) <= 146097/400*J - 693900.28

; Bestimmt zu einem Tag (0 = 1.1.1900) das M�rz-Jahr und den Tag im M�rz-Jahr.
; (Jahr&Tag UTTag) ==> Jahr, Jahrtag
; mit (= UTTag (+ (UTag Jahr) Jahrtag))
(defun Jahr&Tag (UTTag)
  ; Gesucht ist das gr��te Jahr mit UTag(Jahr) <= UTTag.
  ; F�r dieses Jahr J gilt
  ; 146097/400*J - 693902.4775 <= UTag(J) <= UTTag < UTag(J+1) <= 146097/400*J - 693535.0375,
  ; also 146097*J - 277560991 <= 400*UTTag < 146097*J - 277414015,
  ; also 146097*(J-1900) + 23309 <= 400*UTTag < 146097*(J-1900) + 170285,
  ; also J + 0.159544... <= 1900 + UTTag/(146097/400) < J + 1.165561... .
  (let* ((Jahr (+ 1900 (floor (- UTTag 58) 146097/400)))
         (Jahresanfang (UTag Jahr)))
    ; Wegen 146097*(J-1900) + 109 <= 400*(UTTag-58) < 146097*(J-1900) + 147084,
    ; also J <= 1900 + (UTTag-58)/(146097/400) < J+1.006755...,
    ; ist die Sch�tzung  Jahr := floor(1900 + (UTTag-58)/(146097/400))
    ; meist richtig und jedenfalls nicht zu klein und um h�chstens 1 zu gro�.
    (when (< UTTag Jahresanfang) ; zu gro�?
      (decf Jahr)
      (setq Jahresanfang (UTag Jahr))
    )
    (values Jahr (- UTTag Jahresanfang))
) )

; Bei vielen Betriebssystemen (nicht bei UNIX, WIN32) muss die Zeitzone beim
; Installieren in timezone.lsp eingetragen werden. Hier stehen nur
; Defaultwerte.

#-(or UNIX WIN32)
; lokale Zeitzone
(defvar *default-time-zone* -1) ; Default: 1 h �stlich GMT = MEZ
; NB: Zeitzone muss nicht ganzzahlig sein, sollte aber Vielfaches
; einer Sekunde sein.

#-(or UNIX WIN32)
; Funktion, die feststellt, ob bei gegebenem M�rz-Jahr und Tag und Stunde
; Sommerzeit gilt.
(defvar *default-dst-check* ; Default: Sommerzeit nicht explizit bekannt
  #'(lambda (Jahr Jahrtag Stunde) (declare (ignore Jahr Jahrtag Stunde)) nil)
)

; andere Abbildung  Jahrtag -> Monat  f�r decode-universal-time:
; Seien Monat und Jahrtag auf den 1. M�rz bezogen
; (d.h. Jahrtag = 0 am 1. M�rz, = 364 am 28. Februar, usw.,
;  und M�rz=0,...,Dezember=9,Januar=10,Februar=11).
; Dann ist
;                Monat = floor(a*Jahrtag+b)
; sofern a und b so gew�hlt sind, dass die Ungleichungen
;   122*a+b >= 4, 275*a+b >= 9, 30*a+b < 1, 336*a+b < 11
; gelten. Dies ist ein Viereck im Bereich
; 0.032653... = 8/245 <= a <= 7/214 = 0.032710...,
; 0.009345... = 1/107 <= b <= 1/49 = 0.020408...,
; in dem z.B. der Punkt (a=5/153,b=2/153) liegt:
;                Monat = floor((5*Jahrtag+2)/153).

; andere Abbildung  Monat -> Jahrtag
; f�r encode-universal-time und decode-universal-time:
; Seien Monat und Jahrtag auf den 1. M�rz bezogen
; (d.h. Jahrtag = 0 am 1. M�rz, = 364 am 28. Februar, usw.,
;  und M�rz=0,...,Dezember=9,Januar=10,Februar=11).
; Die Abbildung
;      Monat   0  1  2  3  4   5   6   7   8   9   10  11
;      Jahrtag 0 31 61 92 122 153 184 214 245 275 306 337
; kann man schreiben
;                Jahrtag = floor(a*Monat+b)
; sofern a und b so gew�hlt sind, dass die Ungleichungen
;   a+b >= 31, 11*a+b >= 337, 4*a+b < 123, 9*a+b < 276
; gelten. Dies ist ein Viereck im Bereich
; 30.5714... = 214/7 <= a <= 245/8 = 30.625,
; 0.375      = 3/8   <= b <= 5/7   = 0.7142...,
; in dem z.B. der Punkt (a=153/5,b=2/5) liegt:
;                Jahrtag = floor((153*Monat+2)/5).
; Dies ist allerdings langsamer als ein Tabellenzugriff.

(macrolet ((Monat->Jahrtag (Monat) ; 0 <= Monat < 12, 0=M�rz,...,11=Februar
             `(svref '#(0 31 61 92 122 153 184 214 245 275 306 337) ,Monat)
          ))

; (encode-universal-time second minute hour date month year [time-zone]),
; CLTL S. 446
(defun encode-universal-time
              (Sekunde Minute Stunde Tag Monat Jahr &optional (Zeitzone nil)
               &aux Monat3 Jahr3 Jahrtag UTTag)
  (unless (and (and (integerp Jahr)
                    (progn
                      (when (<= 0 Jahr 99)
                        (multiple-value-bind (i1 i2 i3 i4 i5 Jahrjetzt) (get-decoded-time)
                          (declare (ignore i1 i2 i3 i4 i5))
                          (setq Jahr
                            (+ Jahr (* 100 (ceiling (- Jahrjetzt Jahr 50) 100)))
                      ) ) )
                      (<= 1900 Jahr)
               )    )
               (and (integerp Monat) (<= 1 Monat 12))
               (progn
                 (if (< Monat 3)
                   (setq Jahr3 (1- Jahr)  Monat3 (+ Monat 9)) ; Monat3 10..11
                   (setq Jahr3 Jahr       Monat3 (- Monat 3)) ; Monat3 0..9
                 )
                 (and (and (integerp Tag) (<= 1 Tag))
                      (progn
                        (setq Jahrtag (+ (1- Tag) (Monat->Jahrtag Monat3)))
                        (setq UTTag (+ Jahrtag (UTag Jahr3)))
                        (and (if (not (eql Monat3 11))
                               (< Jahrtag (Monat->Jahrtag (1+ Monat3)))
                               (< UTTag (UTag (1+ Jahr3)))
                             )
                             (and (integerp Stunde) (<= 0 Stunde 23))
                             (and (integerp Minute) (<= 0 Minute 59))
                             (and (integerp Sekunde) (<= 0 Sekunde 59))
                             (and (progn
                                    (unless Zeitzone
                                      (setq Zeitzone
                                        #-(or UNIX WIN32)
                                        (- *default-time-zone*
                                           (if (funcall *default-dst-check* Jahr3 Jahrtag Stunde) 1 0)
                                        )
                                        #+(or UNIX WIN32)
                                        (default-time-zone (+ (* 24 UTTag) Stunde))
                                    ) )
                                    (when (floatp Zeitzone) (setq Zeitzone (rational Zeitzone)))
                                    (or (integerp Zeitzone)
                                        (and (rationalp Zeitzone) (integerp (* 3600 Zeitzone)))
                                  ) )
                                  (<= -13 Zeitzone 12)
          )    ) )    ) )    )
    (error-of-type 'error
      (ENGLISH "incorrect date: ~S.~S.~S, ~Sh~Sm~Ss, time zone ~S")
      Tag Monat Jahr Stunde Minute Sekunde Zeitzone
  ) )
  (+ Sekunde
     (* 60 (+ Minute
              (* 60 (+ Stunde Zeitzone
                       (* 24 UTTag)
  )  )     )  )     )
)

; (decode-universal-time universal-time [time-zone]), CLTL S. 445
(defun decode-universal-time (UT &optional (time-zone nil)
                              &aux Sommerzeit Zeitzone)
  (if time-zone
    (setq Sommerzeit nil Zeitzone time-zone)
    #-(or UNIX WIN32)
    (setq time-zone *default-time-zone*
          Sommerzeit (let ((UT (- UT (round (* 3600 time-zone)))))
                       (multiple-value-bind (UTTag Stunde) (floor (floor UT 3600) 24)
                         (multiple-value-bind (Jahr Jahrtag) (Jahr&Tag UTTag)
                           (funcall *default-dst-check* Jahr Jahrtag Stunde)
                     ) ) )
          Zeitzone (- time-zone (if Sommerzeit 1 0))
    )
    #+(or UNIX WIN32)
    (progn
      (multiple-value-setq (Zeitzone Sommerzeit) (default-time-zone (floor UT 3600)))
      (setq time-zone (+ Zeitzone (if Sommerzeit 1 0)))
    )
  )
  ; time-zone = Zeitzone ohne Sommerzeitber�cksichtigung,
  ; Zeitzone = Zeitzone mit Sommerzeitber�cksichtigung.
  (let ((UTSekunden (- UT (round (* 3600 Zeitzone)))))
    (multiple-value-bind (UTMinuten Sekunde) (floor UTSekunden 60)
      (multiple-value-bind (UTStunden Minute) (floor UTMinuten 60)
        (multiple-value-bind (UTTage Stunde) (floor UTStunden 24)
          (multiple-value-bind (Jahr Jahrtag) (Jahr&Tag UTTage)
            (let* ((Monat (floor (+ (* 5 Jahrtag) 2) 153))
                   (Tag (1+ (- Jahrtag (Monat->Jahrtag Monat)))))
              (if (< Monat 10) ; Monat M�rz..Dezember?
                (setq Monat (+ Monat 3)) ; Monat 3..12
                (setq Monat (- Monat 9) Jahr (+ Jahr 1)) ; Monat 1..2
              )
              (values Sekunde Minute Stunde Tag Monat Jahr (mod UTTage 7)
                      Sommerzeit time-zone
) ) ) ) ) ) ) )

) ; Ende von macrolet

; (get-decoded-time), CLTL S. 445
(defun get-decoded-time ()
  (decode-universal-time (get-universal-time))
)


;;; Verschiedenes

; (concat-pnames obj1 obj2) liefert zu zwei Objekten (Symbolen oder Strings)
;  ein Symbol, dessen Printname sich aus den beiden Objekten zusammensetzt.
(defun concat-pnames (obj1 obj2)
  (let ((str (string-concat (string obj1) (string obj2))))
    (if (and (plusp (length str)) (eql (char str 0) #\:))
      (intern (subseq str 1) *keyword-package*)
      (intern str)
) ) )

; Liefert den Typ eines Symbols sym mit (fboundp sym).
(defun fbound-string (sym)
  (cond ((special-operator-p sym)
         (ENGLISH "special form")
        )
        ((functionp (symbol-function sym))
         (ENGLISH "function")
        )
        (t (ENGLISH "macro"))
) )

; *ERROR-HANDLER* should be NIL or a function which accepts the following
; arguments:
; - NIL (in case of ERROR) or a continue-format-string (in case of CERROR),
; - error-format-string,
; - more argument list for these two format strings,
; and which may return only if the first argument is /= NIL.
(defvar *error-handler* nil)

;; Backward compatibility

(sys::%putd 'special-form-p #'special-operator-p)

