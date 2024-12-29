all:
	flex -o mscanner.c scanner.ll
	bison -o mparser.c parser.yy
	g++ -std=c++14 scanner.cpp parser.cpp driver.cc calc.cc -o calc
