all:
	flex -o scanner.cpp scanner.ll
	bison -o parser.cpp parser.yy
	g++ -std=c++14 scanner.cpp parser.cpp driver.cc calc.cc -o calc
