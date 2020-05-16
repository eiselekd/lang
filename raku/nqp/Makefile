all:

test1:
	nqp-m rubyish-1.nqp -e "puts \"test\"\n"

test2:
	nqp-m lisp.nqp -e "(say (+ 1 1))"

dbg:
	nqp-m --debug-port=9999 --debug-suspend lisp.nqp -e "(say (+ 1 1))"

test: test1 test2
