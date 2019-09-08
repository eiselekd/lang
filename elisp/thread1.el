;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html#Backquote
;; ,@ : splice list
;; https://nullprogram.com/blog/2018/05/31/
;;https://emacs-berlin.org/thread-safe-tramp-2018-09.html
;; (all-threads)
;; https://github.com/emacs-mirror/emacs/blob/master/test/src/thread-tests.el

(defvar global-variable 0)
;;(defvar gm (make-mutex))
;;(defvar cv (make-condition-variable gm))

(defun m ()
  (let*
      (( gm (make-mutex))
       ( cv (make-condition-variable gm))
       )
    (setq global-variable 0)
    (make-thread
     (lambda ()
       (progn
	 (let ((a1 (make-mutex)))
	   (message "0: Start ...")
	   (with-mutex gm
	     (message "0: while ...")
       	     (while (< global-variable 10)
	       (message "0: wait %d..." global-variable)
	       (condition-wait cv)))
	   (message "0: Exit...")
	   )
	 )))
    (make-thread
     (lambda ()
       (progn
	 (message "1: Start...")
	 (while (< global-variable 10)
	   (with-mutex gm
	     (setq global-variable (+ global-variable 1))
	     (message "1: set %d..." global-variable)
	     (condition-notify cv)))
	 (message "1: Exit...")
	 )))
    ))

(m)
