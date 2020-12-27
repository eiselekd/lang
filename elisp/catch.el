;;https://steve-yegge.blogspot.com/2008/01/emergency-elisp.html
;;https://github.com/alhassy/ElispCheatSheet

;; * test:
;;  sdsd
;; * test2:
;; ** test
;;    this is test test
;;    this is test test
;;    this is test test
;; ** #
;;    test after
;; * #

(defun f1 (tag val)
  (message (format "thow %s" tag))
  (throw tag val)
  )
(defun f0 (tag val)
  (f1 tag val)
  )

(defun test-0 ()
  (let ((c
	 (catch 'tag0
	   (progn
	     (f0 'tag0 2)
	     (message "returned 1");
	     (progn
	       (message (format "[=] catch found: %s" tag0))
	       )))))
    (message (format "[=] catch found: %s" c))
    
  ))

(test-0)
