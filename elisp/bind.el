
(require 'cl-lib)
(defun a (l)
  (cl-destructuring-bind (x y . rest) l
    (message "x:%s y:%s" x y)))

(a '(1 2 ))
