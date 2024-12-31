all:
	bison -rall -o parse.c n.y
	flex -o scan.c n.l
	sed -i -e 's/static int yy_start/int yy_start/' scan.c
	gcc  -g -c parse.c -o parse.o
	gcc  -g -c scan.c -o scan.o
	gcc  -g -c n.c -o n.o
	gcc  -g scan.o parse.o n.o -lc -o n.exe
	./n.exe test.txt
