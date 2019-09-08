;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html#Backquote
;; ,@ : splice list
;; https://nullprogram.com/blog/2018/05/31/

(defun f0 ()
  (message "f0")
  (message "Exit thread")
  )

(make-thread 'f0 "threadnmame")
