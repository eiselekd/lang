f0:
	flex -L flex0.l
	gcc lex.yy.c -g -o lex0.exe

# Makefile

OBJS	= bison_conflict.o bison_conflict_lex.o
OBJS_NO	= bison_conflict_no.o bison_conflict_no_lex.o

CC	= g++
CFLAGS	= -g -ansi -Wno-write-strings

calc:		calc_yes calc_no

calc_yes:	$(OBJS)
		$(CC) $(CFLAGS) $(OBJS) -o calc_yes -ll -lm
calc_no:	$(OBJS_NO)
		$(CC) $(CFLAGS) $(OBJS_NO) -o calc_no -ll -lm

##########################
## without shift/reduce conflict
bison_conflict_no_lex.o: bison_conflict_no.lex
		flex bison_conflict_no.lex
		$(CC) $(CFLAGS) -c lex.yy.c -o bison_conflict_no_lex.o

bison_conflict_no.o: bison_conflict_no.y
		bison -d -v bison_conflict_no.y
		$(CC) $(CFLAGS) -c bison_conflict_no.tab.c -o bison_conflict_no.o

##########################
## with shift/reduce conflict
bison_conflict_lex.o: bison_conflict.lex
		flex bison_conflict.lex
		$(CC) $(CFLAGS) -c lex.yy.c -o bison_conflict_lex.o


bison_conflict.o: bison_conflict.y
		bison -d -v bison_conflict.y
		$(CC) $(CFLAGS) -c bison_conflict.tab.c -o bison_conflict.o


.PHONY: bison_conflict_no.o bison_conflict.o

clean:
	rm -f *.o *~ calc_yes calc_no calc
