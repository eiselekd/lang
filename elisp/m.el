;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html#Backquote
;; ,@ : splice list

(defmacro addpart (&rest args)
  (let ((s* (and (symbolp (car args))
                 (pop args))))
    `(let* ()
       ,@(if s*
             `((let ((,s* ,s))
		 ,@(cdr args)))
	   (cdr (cdr args)))
       )))

(defun f0 ()
  (addpart (status)
	   (message "Print1")
	   (message "Print2")
	   (message "Print3")))

(f0)
