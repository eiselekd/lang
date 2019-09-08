;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html#Association-Lists
(require 'cl-lib)

(setq l '((pine . cones)
	  (oak . acorns)
	  (maple . seeds)))

(cdr (assoc 'pine l))
(rassoc 'cones l)
(assoc-delete-all 'pine l)
