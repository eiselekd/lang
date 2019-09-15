;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote-Patterns.html

(defun matchsym (n)
  (pcase n
    ('test1 1)
    ('test2 2)
    ))

(matchsym 'test2)
