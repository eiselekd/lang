;;; Common stuff for the demos
;;; Copyright (C) 1999 by Sam Steingold (sds@gnu.org)
;;; GPL2 is applicable

(defpackage clx-demos
  (:use common-lisp)
  (:export qix sokoban))

(in-package :clx-demos)

(defun getenv (var)
  "Return the value of the environment variable."
  #+cmu (cdr (assoc (string var) *environment-list* :test #'equalp
                    :key #'string))
  #-cmu
  (#+(or allegro clisp) system::getenv #+lispworks lw:environment-variable
   #+lucid lcl:environment-variable #+gcl si:getenv (string var)))

(defun x-host-display (&optional (disp (getenv "DISPLAY")))
  "Parse the DISPLAY environment variable.
Return 3 values: host, server, screen."
  (if disp
      (let* ((pos1 (position #\: disp))
             (pos2 (and pos1 (position #\. disp :start pos1))))
        (values (subseq disp 0 pos1)
                (if pos1 (parse-integer (subseq disp (1+ pos1) pos2)) 0)
                (if pos2 (parse-integer (subseq disp (1+ pos2))) 0)))
      (values "" 0 0)))

(defun x-open-display ()
  "Open the appropriate X display."
  (multiple-value-bind (host di) (x-host-display)
    (xlib:open-display host :display di)))

(eval-when (compile)
  (compile-file (merge-pathnames "qix" *compile-file-pathname*))
  (compile-file (merge-pathnames "sokoban" *compile-file-pathname*)))

(load (merge-pathnames "qix" *load-pathname*))
(load (merge-pathnames "sokoban" *load-pathname*))
