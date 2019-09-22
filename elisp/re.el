
(defmacro magit-bind-match-strings (varlist string &rest body)
  "Bind variables to submatches according to VARLIST then evaluate BODY.
Bind the symbols in VARLIST to submatches of the current match
data, starting with 1 and incrementing by 1 for each symbol.  If
the last match was against a string, then that has to be provided
as STRING."
  (declare (indent 2) (debug (listp form body)))
  (let ((s (cl-gensym "string"))
        (i 0))
    `(let ((,s ,string))
       (let ,(save-match-data
               (--map (list it (list 'match-string (cl-incf i) s)) varlist))
         ,@body))))

(defconst magit-log-heading-re
  (concat "^"
          "\\(?4:[-_/|\\*o<>. ]*\\)"               ; graph
          "\\(?1:[0-9a-fA-F]+\\)?\0"               ; sha1
          "\\(?3:[^\0\n]+\\)?\0"                   ; refs
          "\\(?7:[BGUXYREN]\\)?\0"                 ; gpg
          "\\(?5:[^\0\n]*\\)\0"                    ; author
          ;; Note: Date is optional because, prior to Git v2.19.0,
          ;; `git rebase -i --root` corrupts the root's author date.
          "\\(?6:[^\0\n]*\\)\0"                    ; date
          "\\(?2:.*\\)$"))                         ; msg

(defun test_at (style)
  (looking-at (pcase style
                (`log        magit-log-heading-re)
		))
  (magit-bind-match-strings
      (hash msg refs graph author date gpg cherry _ refsub side) nil
    ))

