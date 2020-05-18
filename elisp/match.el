(require 'f)

(defvar dirs '( "/home/eiselekd/tmp/a" "/home/eiselekd/tmp/b" ))

(f-absolute? "a")
(f-full "d")

(defun searchf (p)
  (cond
   ((f-absolute? p)  (f-full p))
   ((f-exists? p) p)
   (t (let* ((retvalue nil))
	(dolist (e dirs retvalue)
	  (let* ((lp (f-join (f-full e) p)))
	    (if (f-exists? lp)
		(setq retvalue lp))))))))

(searchf "file.c")







(defun searchfile ()
  (let*
      ((retvalue nil))
    (dolist (e dirs retvalue)
      (message "%s" e))))

(searchfile)

(defun test ()
  (let* ((buffer (get-buffer-create "match")))
    (with-current-buffer buffer
      (progn
	(erase-buffer)
	(insert-file-contents "match.txt" nil)
	(goto-char (point-min))

	(when (re-search-forward
               (rx (and line-start
			(1+ space) "0"
			(1+ space)
			(1+ (not space))
			(1+ space)
			(group-n 1 (1+ (not ":")))
			":"
			(group-n 2 (1+ digit))))
               nil t)
	  (let ((filename (match-string 1))
		(line (string-to-number (match-string 2))))
            (message (format "Found file: %s, line: %d" filename line))
            )
	)))))

(test)
;;
