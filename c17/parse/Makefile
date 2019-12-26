PSRCS= parse.cpp \
       lexer.cpp


#       lang_y.cpp \
#       lang_l.cpp \

ifeq ($(USEENV),clang)
# until c++17 is supported:
CC=clang++-4.0  -std=c++14
CXX=clang++-4.0 -std=c++14
else
CC=g++-7   -std=c++17
CXX=g++-7  -std=c++17
endif

CFLAGS= -g
TESTS=test test-lexer
ADDLIB=-Wl,--whole-archive lib/libparse.a -Wl,--no-whole-archive -lgtest -lpthread

POBJS=$(foreach f,$(patsubst %.c,%.o,$(patsubst %.cpp,%.o,$(PSRCS))),.obj/$(f))
EOBJS=$(foreach f,$(TESTS),$(f).exe)

all: $(EOBJS)

lib/libparse.a: $(POBJS)
	@mkdir -p lib
	$(CROSS-COMPILE)ar cr $@ $^; $(CROSS-COMPILE)ranlib $@
	@echo "$@: $^" > $@.dep

.obj/%.o: %.c
	@-mkdir -p $(dir $@)
	$(CROSS-COMPILE)$(CC) $(CFLAGS) $(DEBUG_F) $(TRACING)  $(ADDINC) -c $< -o $@
	@echo -n "$@:"                                                                                   > $@.dep
	@$(CROSS-COMPILE)$(CC) $(CFLAGS) $(DEBUG_F) $(TRACING)  $(ADDINC)  -c $< -MM | sed -e 's/.*://' >> $@.dep

.obj/%.o: %.cpp
	@-mkdir -p $(dir $@)
	$(CROSS-COMPILE)$(CXX) $(CFLAGS)  $(DEBUG_F) $(TRACING) $(ADDINC) -c $< -o $@
	@echo -n "$@:"                                                                                   >$@.dep
	@$(CROSS-COMPILE)$(CXX) $(CFLAGS)  $(DEBUG_F) $(TRACING) $(ADDINC) -c $< -MM | sed -e 's/.*://' >> $@.dep

%.exe: %.c lib/libparse.a
	$(CROSS-COMPILE)$(CC) $(DEBUG_F) $(TRACING)  $(ADDINC)  $< $(ADDLIB) -o $@
	@echo -n "$@:"                                                                                 > $@.dep
	@$(CROSS-COMPILE)$(CC) $(DEBUG_F) $(TRACING)  $(ADDINC)  $< $(ADDLIB) -MM   | sed -e 's/.*://' >> $@.dep

%.exe: %.cpp lib/libparse.a
	$(CROSS-COMPILE)$(CXX) $(CFLAGS) $(DEBUG_F) $(TRACING)  $(ADDINC) $< $(ADDLIB)  -o $@
	@echo -n "$@:"                                                                                 > $@.dep
	@$(CROSS-COMPILE)$(CXX) $(CFLAGS) $(DEBUG_F) $(TRACING)  $(ADDINC) $< $(ADDLIB) -MM   | sed -e 's/.*://' >> $@.dep


%.cpp : %.lex
	flex -o $@ $<

lang_l.cpp : lang_y.cpp

%.cpp : %.y
	bison -d -o $@ $<


clean:
	-rm -rf .obj/*.o lib/*.a .obj/*.dep lib/*.dep

test:
	make all
	./test.exe

test_dllist:
	$(CC) -g dllist.cpp -o dllist.exe

-include $(wildcard .obj/*dep) $(wildcard bin/*dep) $(wildcard *dep)

gtest-lexer: test-lexer.exe
	./test-lexer.exe


prep-flex:
	cd flex-2.6.4; ./configure --prefix=$(CURDIR)/bin-flex; make; make install

.PHONY: lang_l_simple.c

lang_l_simple.c: lang_l_simple.lex
	$(CURDIR)/bin-flex/bin/flex -x -o lang_l_simple.c lang_l_simple.lex
	$(CURDIR)/bin-flex/bin/flex -o lang_l_simple.reference.c lang_l_simple.lex
