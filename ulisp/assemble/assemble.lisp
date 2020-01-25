;
; ARM Thumb Assembler v4 - 24th January 2020
; see http://www.ulisp.com/show?2XZH
;

(defvar *pc* 0)

; Print 16-bit number in hex
(defun x (n)
  (princ "#x")
  (dotimes (j 4 nothing)
    (let ((d (logand (ash n (- (* j 4) 12)) #xf)))
      (princ
       (code-char (+ d (if (< d 10) (char-code #\0) (char-code #\W))))))))

; Extract register number
(defun regno (symbol)
  (- (char-code (char (string symbol) 1)) (char-code #\0)))

; Pack arguments into bit fields
(defun emit (bits &rest args)
  (let ((word 0))
    (unless (= (apply #'+ bits) 16) (error "Incorrect number of bits"))
    (mapc #'(lambda (width value)
              (when (> value (1- (expt 2 width))) (error "Won't fit"))
              (setq word (logior (ash word width) value)))
          bits args)
    (incf *pc* 2)
    word))

; Errors
(defun error (txt) (print txt))

; label

(defun $label (var)
  (set var *pc*)
  nil)

; Shared routines, ordered by first four bits

; lsl lsr 0

(defun lsl-lsr-0 (op immed5 argm argd)
  (emit '(4 1 5 3 3) 0 op immed5 (regno argm) (regno argd)))

; add sub 1

(defun add-sub-1 (op argd argn argm)
  (cond
   ((numberp argm)
    (emit '(4 1 1 1 3 3 3) 1 1 1 op argm (regno argn) (regno argd)))
   ((null argm)
    (emit '(4 1 1 1 3 3 3) 1 1 0 op (regno argn) (regno argd) (regno argd)))
   (t
    (emit '(4 1 1 1 3 3 3) 1 1 0 op (regno argm) (regno argn) (regno argd)))))

; mov cmp 2

(defun mov-cmp-2 (op argd immed8)
  (emit '(4 1 3 8) 2 op (regno argd) immed8))

; add sub 3

(defun add-sub-3 (op argd immed8)
  (emit '(4 1 3 8) 3 op (regno argd) immed8))

; add mov 4

(defun add-mov-4 (op argd argm)
  (let ((rd (regno argd))
        (rm (regno argm)))
    (cond
     ((and (>= rd 8) (>= rm 8))
      (emit '(4 2 1 1 2 3 3) 4 1 op 0 3 (- rm 8) (- rd 8)))
     ((>= rm 8)
      (emit '(4 2 1 1 2 3 3) 4 1 op 0 1 (- rm 8) rd))
     ((>= rd 8)
      (emit '(4 2 1 1 2 3 3) 4 1 op 0 2 rm (- rd 8))))))

; and to mvn 4

(defun and-mvn-4 (op argd argm)
  (emit '(4 1 5 3 3) 4 0 op (regno argm) (regno argd)))

; bx blx 4

(defun bx-blx (op argm)
  (emit '(4 1 3 1 4 3) 4 0 7 op (regno argm) 0))

; str ldr 4, 6, 9

(defun str-ldr (op argd arg2)
  (cond
   ((listp arg2)
    (let ((argn (first arg2))
          (immed (second arg2)))
      (unless (zerop (mod immed 4)) (error "Immediate argument not multiple of 4"))
      (cond
       ((eq argn 'pc)
        (when (= op 0) (error "str not allowed with pc"))
        (emit '(4 1 3 8) 4 1 (regno argd) (truncate immed 4)))
       ((eq argn 'sp)
        (emit '(4 1 3 8) 9 op (regno argd) (truncate immed 4)))
       (t
        (emit '(4 1 5 3 3) 6 op (truncate immed 4) (regno argn) (regno argd))))))))

; push pop 11

(defun push-pop (op list)
  (let ((byte 0)
        (r 0))
    (mapc #'(lambda (x) 
              (cond
               ((and (= op 0) (eq x 'lr)) (setq r 1))
               ((and (= op 1) (eq x 'pc)) (setq r 1))
               (t (setq byte (logior byte (ash 1 (regno x))))))) list)
    (emit '(4 1 2 1 8) 11 op 2 r byte)))

; b cond 13

(defun b-cond-13 (cond label)
  (let ((soff8 (logand (ash (- (eval label) *pc* 4) -1) #xff)))
    (emit '(4 4 8) 13 cond soff8)))

; Alphabetical list of mnemonics

(defun $adc (argd argm)
  (and-mvn-4 5 argd argm))

(defun $add (argd argn &optional argm)
  (cond
   ((numberp argn)
    (add-sub-3 0 argd argn))
   (t
    (add-sub-1 0 argd argn argm))))

(defun $and (argd argm)
  (and-mvn-4 0 argd argm))

(defun $asr (argd argm)
  (and-mvn-4 4 argd argm))

(defun $b (label)
  (let ((offset (ash (- (eval label) *pc* 4) -1)))
    (emit '(4 1 11) #xe 0 (logand offset #x7ff))))

(defun $bcs (label)
  (b-cond-13 2 label))

(defun $beq (label)
  (b-cond-13 0 label))

(defun $bge (label)
  (b-cond-13 10 label))

(defun $bgt (label)
  (b-cond-13 12 label))

(defun $ble (label)
  (b-cond-13 13 label))

(defun $blt (label)
  (b-cond-13 11 label))

(defun $bmi (label)
  (b-cond-13 4 label))

(defun $bne (label)
  (b-cond-13 1 label))

(defun $bic (argd argm)
  (and-mvn-4 14 argd argm))

(defun $bl (label)
  (let ((offset (ash (- (eval label) *pc* 4) -1)))
    (list
     (emit '(4 1 11) #xf 0 (logand (ash offset -11) #x7ff))
     (emit '(4 1 11) #xf 1 (logand offset #x7ff)))))

(defun $blx (argm)
  (bx-blx 1 argm))

(defun $bx (argm)
  (bx-blx 0 argm))

(defun $cmn (argd argm)
  (and-mvn-4 11 argd argm))

(defun $cmp (argd argm)
  (cond
   ((numberp argm)
    (mov-cmp-2 1 argd argm))
   (t
    (and-mvn-4 10 argd argm))))
    
(defun $eor (argd argm)
  (and-mvn-4 1 argd argm))

(defun $ldr (argd arg2)
  (str-ldr 1 argd arg2))

(defun $lsl (argd argm &optional arg2)
  (cond
   ((numberp arg2)
    (lsl-lsr-0 0 arg2 argm argd))
   ((numberp argm)
    (lsl-lsr-0 0 argm argd argd))
   (t
    (and-mvn-4 2 argd argm))))

(defun $lsr (argd argm &optional arg2)
  (cond
   ((numberp arg2)
    (lsl-lsr-0 1 arg2 argm argd))
   ((numberp argm)
    (lsl-lsr-0 1 argm argd argd))
   (t
    (and-mvn-4 3 argd argm))))

(defun $mov (argd argm)
  (cond
   ((numberp argm)
    (mov-cmp-2 0 argd argm))
   (t ; Synonym of LSLS Rd, Rm, #0
    (lsl-lsr-0 0 0 argm argd))))

(defun $mul (argd argm)
  (and-mvn-4 13 argd argm))

(defun $mvn (argd argm)
  (and-mvn-4 15 argd argm))

(defun $neg (argd argm)
  (and-mvn-4 9 argd argm))

(defun $nop () ; mov r8,r8
  (add-mov-4 1 'r8 'r8))

(defun $orr (argd argm)
  (and-mvn-4 12 argd argm))

(defun $push (list)
  (push-pop 0 list))

(defun $pop (list)
  (push-pop 1 list))

(defun $ror (argd argm)
  (and-mvn-4 7 argd argm))

(defun $sbc (argd argm)
  (and-mvn-4 6 argd argm))

(defun $str (argd arg2)
  (str-ldr 0 argd arg2))

(defun $sub (argd argn &optional argm)
  (cond
   ((numberp argn)
    (add-sub-3 1 argd argn))
   (t
    (add-sub-1 1 argd argn argm))))

(defun $tst (argd argm)
  (and-mvn-4 8 argd argm))