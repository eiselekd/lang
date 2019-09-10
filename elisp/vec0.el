;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html#Backquote
;; ,@ : splice list
;; https://nullprogram.com/blog/2018/05/31/
;;https://emacs-berlin.org/thread-safe-tramp-2018-09.html
;; (all-threads)
;; https://github.com/emacs-mirror/emacs/blob/master/test/src/thread-tests.el

(setq v0 [ 0 nil 10 2 3] )

(setq v1 (seq-filter (lambda (elt) (not (eq elt nil))) v0))
(sort v1 '< )

(let ((i 1)
      (j 0))
  `( ,i  ,j))O


(mapcar (lambda (a) (message "%d" a)) v0)

(condition-case nil
    (elt v0 2)
  (error (message "test")))
