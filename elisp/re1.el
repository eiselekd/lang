
(setq m
"➜  lang git:(master) ✗ python3 -m unittest python/unittest/test.py
F.F
======================================================================
FAIL: test_isupper (python.unittest.test.TestStringMethods)
----------------------------------------------------------------------
Traceback (most recent call last):
  File \"/home/eiselekd/git/lang/python/unittest/test.py\", line 19, in test_isupper
    self.assertTrue('FOo'.isupper())
AssertionError: False is not true

======================================================================
FAIL: test_upper (python.unittest.test.TestStringMethods)
----------------------------------------------------------------------
Traceback (most recent call last):
  File \"/home/eiselekd/git/lang/python/unittest/test.py\", line 16, in test_upper
    self.subf()
  File \"/home/eiselekd/git/lang/python/unittest/test2.py\", line 13, in subf
    self.assertEqual('foo'.upper(), 'FOo')
AssertionError: 'FOO' != 'FOo'
- FOO
?   ^
+ FOo
?   ^


----------------------------------------------------------------------
Ran 3 tests in 0.001s

")

(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 1 string) matches)
        (setq pos (match-end 0)))
      (nreverse matches))))

(setq regexp (rx
	      "======================================================================\n"
	      "FAIL: " (group-n 3 (one-or-more not-newline)) "\n"
	      "----------------------------------------------------------------------\n"
	      "Traceback" (one-or-more not-newline) "\n"
	      (zero-or-more (or "  " "    ") (one-or-more not-newline) "\n" )
	      "  File \"" (group-n 1 (minimal-match (one-or-more not-newline)))  "\", line " (group-n 2 (one-or-more ditit))  (one-or-more not-newline) "\n"
	      "    " (one-or-more not-newline) "\n"
	      "AssertionError"
	      ))


(re-seq regexp m)
