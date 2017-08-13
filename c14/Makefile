all: bison
	g++ -std=c++14 0_lambda.cpp -o 0_lambda.exe; ./0_lambda.exe
	g++ -std=c++14 0_bind.cpp -o 0_bind.exe; ./0_bind.exe
	g++ -std=c++14 1_thread.cpp -o 1_thread.exe -lpthread; ./1_thread.exe
	g++ -std=c++14 2_switch.cpp -o 2_switch.exe; ./2_switch.exe

var:
	g++ -std=c++14 3_var.cpp -o 3_var.exe; ./3_var.exe

l4.exe: l4.o l4main.c
	cc  l4.o l4main.c -o l4.exe
