PSRCS=  \

ifeq ($(USEENV),clang)
# until c++17 is supported:
CC=clang++-4.0  -std=c++14
CXX=clang++-4.0 -std=c++14
else
CC=g++-7   -std=c++17
CXX=g++-7  -std=c++17
endif

ADDLIB=-Llib -lhash

CFLAGS= -g
TESTS=test

POBJS=$(foreach f,$(patsubst %.c,%.o,$(patsubst %.cpp,%.o,$(PSRCS))),.obj/$(f))
EOBJS=$(foreach f,$(TESTS),$(f).exe)

all: lib/libhash.a $(EOBJS)

lib/libhash.a: $(POBJS)
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

%.exe: %.c lib/libhash.a
	$(CROSS-COMPILE)$(CC) $(DEBUG_F) $(TRACING)  $(ADDINC)  $< $(ADDLIB) -o $@
	@echo -n "$@:"                                                                                 > $@.dep
	@$(CROSS-COMPILE)$(CC) $(DEBUG_F) $(TRACING)  $(ADDINC)  $< $(ADDLIB) -MM   | sed -e 's/.*://' >> $@.dep

%.exe: %.cpp lib/libhash.a
	$(CROSS-COMPILE)$(CXX) $(CFLAGS) $(DEBUG_F) $(TRACING)  $(ADDINC) $< $(ADDLIB)  -o $@
	@echo -n "$@:"                                                                                 > $@.dep
	@$(CROSS-COMPILE)$(CXX) $(CFLAGS) $(DEBUG_F) $(TRACING)  $(ADDINC) $< $(ADDLIB) -MM   | sed -e 's/.*://' >> $@.dep

clean:
	-rm -rf .obj/*.o lib/*.a .obj/*.dep lib/*.dep

test:
	make all
	./test.exe

-include $(wildcard .obj/*dep) $(wildcard bin/*dep) $(wildcard *dep)
