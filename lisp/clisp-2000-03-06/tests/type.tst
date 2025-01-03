
(TYPEP (QUOTE A) (QUOTE SYMBOL))
T

(TYPEP (QUOTE NIL) (QUOTE SYMBOL))
T

(TYPEP (QUOTE (NIL)) (QUOTE SYMBOL))
NIL

(TYPEP 3 (QUOTE INTEGER))
T

(TYPEP 3 (QUOTE (INTEGER 0 4)))
T

(TYPEP 3 (QUOTE (INTEGER 0 3)))
T

(TYPEP 3 (QUOTE (INTEGER 0 2)))
NIL

(TYPEP 3 (QUOTE (FLOAT 0.0 2.0)))
NIL

(TYPEP 3 (QUOTE (FLOAT 0.0 2.0)))
NIL

(TYPEP 3 (QUOTE (FLOAT 0.0 4.0)))
NIL

(TYPEP 3.2 (QUOTE (FLOAT 0.0 4.0)))
T

(TYPEP 3.2 (QUOTE (FLOAT 0.0 3.2)))
T

(TYPEP 3.2 (QUOTE (FLOAT 0.0 (3.2))))
NIL

(TYPEP 3.2 (QUOTE (SHORT-FLOAT 0.0S0 3.2S0)))
#+(or ALLEGRO CMU) T #-(or ALLEGRO CMU) NIL

(TYPEP 3.2 (QUOTE (SINGLE-FLOAT 0.0F0 3.2F0)))
T

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2S0 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (FLOAT 0.0 3.2)))
T

(TYPEP 3.2S0 (QUOTE (FLOAT 0.0S0 3.2S0)))
T

(TYPEP 2.0S0 (QUOTE (SHORT-FLOAT 0.0S0 3.0S0)))
T

(TYPEP 2.0S0 (QUOTE (SINGLE-FLOAT 0.0F0 3.0F0)))
#+(or ALLEGRO CMU) T #-(or ALLEGRO CMU) NIL

(TYPEP 2.0 (QUOTE (SINGLE-FLOAT 0.0F0 3.0F0)))
T

(TYPEP 2.0D0 (QUOTE (DOUBLE-FLOAT 0.0D0 3.0D0)))
T

(TYPEP 3.0D0 (QUOTE (DOUBLE-FLOAT 0.0D0 3.0D0)))
T

(TYPEP 3.0D0 (QUOTE (DOUBLE-FLOAT 0.0D0 (3.0D0))))
NIL

(TYPEP 4 (QUOTE (MOD 4)))
NIL

(TYPEP 4 (QUOTE (MOD 5)))
T

(TYPEP 4 (QUOTE (RATIONAL 2 5)))
T

(TYPEP 4 (QUOTE (RATIONAL 2 7/2)))
NIL

(TYPEP 4 (QUOTE (RATIONAL 2 9/2)))
T

(TYPEP 4 (QUOTE (RATIONAL 2 4)))
T

(TYPEP 4/3 (QUOTE (RATIONAL 2 4)))
NIL

(TYPEP 2 (QUOTE (RATIONAL 2 4)))
T

(TYPEP "abcd" (QUOTE STRING))
T

(TYPEP "abcd" (QUOTE (STRING 4)))
T

(TYPEP "abcd" (QUOTE (STRING 43)))
NIL

(TYPEP '#(2 3) (QUOTE (COMPLEX INTEGER)))
NIL

(TYPEP '#(2 3) (QUOTE COMPLEX))
NIL

(TYPEP #C(2 3) (QUOTE COMPLEX))
T

(TYPEP #C(2 3) (QUOTE (COMPLEX INTEGER)))
T

(TYPEP #C(2 3) (QUOTE (COMPLEX FLOAT)))
NIL

(TYPEP #C(2 3) (QUOTE (COMPLEX SYMBOL)))
#+CMU ERROR #-CMU NIL

(TYPEP '#(A B C D) (QUOTE VECTOR))
T

(TYPEP '#(A B C D) (QUOTE (VECTOR * 4)))
T

#| ; h�ngt von (upgraded-array-element-type 'SYMBOL) ab!
(TYPEP '#(A B C D) (QUOTE (VECTOR SYMBOL 4)))
NIL
|#

(TYPEP (QUOTE A) (QUOTE (SYMBOL CONS)))
ERROR

(TYPEP (QUOTE A) (QUOTE (OR CONS SYMBOL)))
T

(TYPEP (QUOTE A) (QUOTE (OR CONS NUMBER)))
NIL

(TYPEP (QUOTE A) (QUOTE (OR ATOM NUMBER)))
T

(TYPEP (QUOTE A) (QUOTE (AND ATOM NUMBER)))
NIL

(TYPEP (QUOTE 2) (QUOTE (AND ATOM NUMBER)))
T

(TYPEP (QUOTE 2) (QUOTE (MEMBER 1 2 3)))
T

(TYPEP (QUOTE 2) (QUOTE (MEMBER 1 3)))
NIL

(TYPEP (QUOTE 2) (QUOTE (NOT (MEMBER 1 3))))
T

(TYPEP (QUOTE 2) (QUOTE (NOT (MEMBER 1 2 3))))
NIL

(TYPEP 2 (QUOTE (AND NUMBER (NOT SYMBOL))))
T

(TYPEP 2 (QUOTE (AND STRING (NOT SYMBOL))))
NIL

(TYPEP 2 (QUOTE (OR STRING (NOT SYMBOL))))
T

(TYPEP (QUOTE CONS) (QUOTE FUNCTION))
#-(or CLtL2 ANSI-CL CLISP) T #+(or CLtL2 ANSI-CL CLISP) NIL

(TYPEP (QUOTE CONS) (QUOTE (SATISFIES FUNCTIONP)))
#-(or CLtL2 ANSI-CL CLISP) T #+(or CLtL2 ANSI-CL CLISP) NIL

(TYPEP (QUOTE CONS) (QUOTE (SATISFIES NOT)))
NIL

(TYPEP (QUOTE NIL) (QUOTE (SATISFIES NOT)))
T

(TYPEP (QUOTE NIL) NIL)
NIL

(TYPEP (QUOTE T) NIL)
NIL

(SUBTYPEP (QUOTE CONS) T)
T

(SUBTYPEP NIL (QUOTE CONS))
T

(SUBTYPEP (QUOTE CONS) (QUOTE LIST))
T

(SUBTYPEP (QUOTE CONS) (QUOTE (OR ATOM CONS)))
T

(SUBTYPEP (QUOTE CONS) (QUOTE (AND ATOM CONS)))
NIL

(SUBTYPEP (QUOTE CONS) (QUOTE (NOT ATOM)))
#-(or CLISP AKCL ALLEGRO) T #+(or CLISP AKCL ALLEGRO) NIL

(SUBTYPEP (QUOTE LIST) (QUOTE (NOT ATOM)))
NIL

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (INTEGER 0 7)))
T

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (INTEGER 0 (5))))
NIL

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (INTEGER 0 5)))
T

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (MOD 5)))
NIL

(SUBTYPEP (QUOTE (INTEGER 1 (5))) (QUOTE (MOD 5)))
T

(SUBTYPEP (QUOTE (OR (INTEGER 1 (5) FLOAT))) (QUOTE (OR FLOAT (MOD
5))))
#+(or XCL CLISP) T #+(or ALLEGRO CMU) ERROR #-(or XCL CLISP ALLEGRO CMU) UNKNOWN

(SUBTYPEP (QUOTE (OR (INTEGER 1 (5)) FLOAT)) (QUOTE (OR FLOAT (MOD
5))))
T

(SUBTYPEP (QUOTE (AND NUMBER (FLOAT 1.0 (5.0)))) (QUOTE (OR FLOAT (MOD
5))))
T

(SUBTYPEP (QUOTE (AND NUMBER (NOT (FLOAT 1.0 (5.0)))))
(QUOTE (OR FLOAT (MOD 5))))
NIL


(SUBTYPEP (QUOTE (AND FLOAT (NOT (FLOAT 1.0 (5.0))))) (QUOTE (OR FLOAT
(MOD 5))))
T

(SUBTYPEP (QUOTE (AND FLOAT (NOT (FLOAT 1.0 (5.0)))))
(QUOTE (OR (FLOAT * 1.0) (FLOAT * 5.0))))
NIL

(SUBTYPEP (QUOTE (SATISFIES CONSP)) (QUOTE LIST))
NIL

(SUBTYPEP (QUOTE SIMPLE-STRING) (QUOTE ARRAY))
T

(DEFTYPE MOD1 (N) `(AND NUMBER (FLOAT 0.0 (,N))))
MOD1

(TYPEP 4.1 (QUOTE (MOD1 5.0)))
T

(TYPEP 4.1 (QUOTE (MOD1 4.1)))
NIL

(SUBTYPEP (QUOTE (FLOAT 2.3 6.7)) (QUOTE (MOD1 6.8)))
T

(SUBTYPEP (QUOTE (FLOAT 2.3 6.7)) (QUOTE (MOD1 6.7)))
NIL

(DEFUN BELIEBIGER-TEST (A) (MEMBER A (QUOTE (U I V X))))
BELIEBIGER-TEST

(NOT (NULL (TYPEP (QUOTE U) (QUOTE (SATISFIES BELIEBIGER-TEST)))))
T

(TYPEP (QUOTE A) (QUOTE (SATISFIES BELIEBIGER-TEST)))
NIL

(SUBTYPEP (QUOTE (MEMBER U I)) (QUOTE (SATISFIES BELIEBIGER-TEST)))
T

(SUBTYPEP (QUOTE (OR (MEMBER U I))) (QUOTE (SATISFIES BELIEBIGER-TEST)))
T

(SUBTYPEP (QUOTE (OR (MEMBER U I A))) (QUOTE (SATISFIES BELIEBIGER-TEST)))
NIL

(SUBTYPEP (QUOTE (SATISFIES BELIEBIGER-TEST)) (QUOTE (MEMBER U I V
X Y)))
NIL

(DEFTYPE BELIEBIGER-TYP NIL (QUOTE (SATISFIES BELIEBIGER-TEST)))
BELIEBIGER-TYP

(NOT (NULL (TYPEP (QUOTE U) (QUOTE BELIEBIGER-TYP))))
T

(TYPEP (QUOTE A) (QUOTE BELIEBIGER-TYP))
NIL

(SUBTYPEP (QUOTE (MEMBER U I)) (QUOTE BELIEBIGER-TYP))
T

(SUBTYPEP (QUOTE BELIEBIGER-TYP) (QUOTE (MEMBER U I V X Y)))
NIL
(subtypep nil 'fixnum) t
(subtypep 'short-float 'float ) t
(subtypep 'single-float 'float ) t
(subtypep 'double-float 'float ) t
(subtypep 'long-float 'float ) t

(subtypep 'null 'symbol) t
(subtypep 'null 'list) t
(subtypep 'cons 'list) t

(subtypep 'standard-char 'string-char) t

(subtypep 'string-char 'character) t

(subtypep 'string 'vector) t

(subtypep 'bit-vector 'vector) t
(subtypep 'vector 'array) t

(subtypep 'simple-array 'array) t

(subtypep 'simple-vector 'simple-array) t
(subtypep 'simple-vector 'vector) t
(subtypep 'simple-string 'simple-array) t
(subtypep 'simple-bit-vector 'simple-array) t

(subtypep 'simple-string 'string) t
(subtypep 'simple-string 'vector) t
(subtypep 'simple-string 'simple-vector) nil
(subtypep 'simple-bit-vector 'bit-vector) t
(subtypep 'bit-vector 'vector) t
(subtypep 'simple-bit-vector 'simple-vector) nil

(subtypep 'unsigned-byte 'integer) t
(subtypep 'signed-byte 'integer) t
