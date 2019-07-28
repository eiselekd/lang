all:
	ocamlopt -c ocaml-called-from-c.ml -o ocaml-called-from-c.cmx
	ocamlopt -output-obj -o camlcode.o ocaml-called-from-c.cmx
	gcc -g -Wall -Wextra  -c c-main-calls-ocaml.c -o c-main-calls-ocaml.o
	gcc camlcode.o c-main-calls-ocaml.o -lm -L /usr/lib/ocaml/ \
         -lasmrun -ldl -o c-main-calls-ocaml
	./c-main-calls-ocaml 4 abc wxyz
