all: bison
	g++ -std=c++14 0_lambda.cpp -o 0_lambda.exe; ./0_lambda.exe
	g++ -std=c++14 0_bind.cpp -o 0_bind.exe; ./0_bind.exe
	g++ -std=c++14 1_thread.cpp -o 1_thread.exe -lpthread; ./1_thread.exe
	g++ -std=c++14 2_switch.cpp -o 2_switch.exe; ./2_switch.exe

conv:
	g++ -g -std=c++14 13_conv.cpp -o 13_conv.exe -lpthread; ./13_conv.exe

pack:
	g++ -g -std=c++14 10_temppack.cpp -o 10_temppack.exe -lpthread; ./10_temppack.exe

p:
	g++ -g -std=c++14 11_assign.cpp -o 11_assign.exe -lpthread; ./11_assign.exe


thread:
	g++ -std=c++14 1_thread_2.cpp -o 1_thread_2.exe -lpthread; ./1_thread_2.exe

temppack:
	g++ -std=c++14 9_temppack.cpp -o 9_temppack.exe; ./9_temppack.exe

var:
	g++ -std=c++14 3_var.cpp -o 3_var.exe; ./3_var.exe
	g++ -std=c++14 4_var.cpp -o 4_var.exe; ./4_var.exe

var5:
	g++ -std=c++14 5_var_data.cpp -o 5_var_data.exe; ./5_var_data.exe

var6:
	g++ -std=c++14 6_var.cpp -o 6_var_data.exe; ./6_var_data.exe

var7:
	g++ -std=c++14 7_forward.cpp -o 7_forward.exe; ./7_forward.exe

# var8:
# 	g++ -std=c++14 8_enable.cpp -o 8_enable.exe; ./8_enable.exe


var5-clang:
	clang++ -std=c++14 -Xclang -fdump-record-layouts 5_var_data.cpp -o 5_var_data.exe; ./5_var_data.exe

l4.exe: l4.o l4main.c
	cc  l4.o l4main.c -o l4.exe
