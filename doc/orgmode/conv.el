(require 'cli (concat (file-name-directory load-file-name) "org-export-cli.el"))

;; (byte-compile-file (concat (file-name-directory load-file-name) "cli.el"))
(setq options-alist
      '(("--infile" "path to input .org file (required)")
	))

(setq args (cli-parse-args options-alist "
Note that code block evaluation is disabled by default; use
'--evaluate' to set a default value of ':eval yes' for all code
blocks. If you would like to evaluate by default without requiring
this option, include '#+PROPERTY: header-args :eval yes' in the file
header. Individual blocks can be selectively evaluated using ':eval
yes' in the block header.
"))
(defun getopt (name) (gethash name args))

(message "[+] conv.el loaded")

(require 'ox)
(require 'ox-html)
(require 'ox-twbs (concat (file-name-directory load-file-name) "ox-twbs.el"))
(message "[+] org loaded")

(defvar infile (getopt "infile"))
(defvar outfile
  (file-truename
   (or (getopt "outfile") (replace-regexp-in-string "\.org$" ".html" infile))))
(message "[+] infile '%s' -> '%s'" infile outfile)

(defvar infile-temp (make-temp-name (format "%s.temp." infile)))
(copy-file infile infile-temp t)
(find-file infile-temp)

(org-mode)
(message (format "org-mode version %s" org-version))
(org-twbs-export-as-html)

(write-file outfile)

