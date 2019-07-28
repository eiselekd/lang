
############ ocaml ##############

O_MLS= \
	common.ml \
	token.ml \
	lex.ml \
	main.ml

OTESTS=test

ML_INCS := -I . -I .obj
ML_NATIVE_LIBS := unix.cmxa nums.cmxa bigarray.cmxa
OCAMLC_FLAGS := -bin-annot -g $(ML_INCS) -w Ael
OCAMLOPT_FLAGS := -bin-annot $(ML_INCS) -w Ael
OPT=.opt
MKFILES := Makefile

O_CMOS := $(addprefix .obj/, $(O_MLS:.ml=.cmo))
O_CMXS := $(addprefix .obj/, $(O_MLS:.ml=.cmx))
O_OBJS := $(addprefix .obj/, $(O_MLS:.ml=.o))
O_CMIS := $(addprefix .obj/, $(O_MLS:.ml=.cmi))

ML_DEPFILES := $(addprefix .obj/, $(O_MLS:.ml=.dep))


all: $(MKFILES) $(O_CMXS) $(OTESTS:=.oexe)
#	echo $(O_CMXS)

.obj/%.cmo: %.ml
	@$(call CFG_ECHO, compile: $<)
	$(CFG_QUIET)ocamlc$(OPT) -c -o $@ $(OCAMLC_FLAGS) $<

.obj/%.cmo: %.cmi

.obj/%.cmx .obj/%.o: %.ml
	@$(call CFG_ECHO, compile: $<)
	$(CFG_QUIET)ocamlopt$(OPT) -c -o $@ $(OCAMLOPT_FLAGS) $<

%.ml: %.mll
	@$(call CFG_ECHO, lex-gen: $<)
	$(CFG_QUIET)ocamllex$(OPT) -q -o $@ $<

.obj/%.dep: %.ml $(MKFILES)
	@$(call CFG_ECHO, dep: $<)
	$(CFG_QUIET)ocamldep$(OPT) $(ML_INCS) $< $(CFG_PATH_MUNGE) >$@

.obj/%.dep: %.mli $(MKFILES)
	@$(call CFG_ECHO, dep: $<)
	$(CFG_QUIET)ocamldep$(OPT) $(ML_INCS) $< $(CFG_PATH_MUNGE) >$@

%.oexe: $(O_CMXS)
	@$(call CFG_ECHO, compile: $<)
	$(CFG_QUIET)ocamlopt$(OPT) -o $@ $(OCAMLOPT_FLAGS) $(ML_NATIVE_LIBS) $(O_CMXS)

clean:
	-rm -rf .obj/*.o lib/*.a .obj/*.dep lib/*.dep \
		.obj/*.cmi .obj/*.cmt .obj/*.cmx *.oexe

test:
	make all
	./test.oexe data.txt

test_dllist:
	$(CC) -g dllist.cpp -o dllist.exe

-include $(ML_DEPFILES) $(wildcard bin/*dep) $(wildcard *dep)


control0.oexe: control0.ml
	$(CFG_QUIET)ocamlfind ocamlopt -linkpkg -package fmt -o $@ control0.ml
