
#CC=clang++ -std=c++1z
#CXX=clang++ -std=c++1z

all: bison
	g++ -std=c++14 0_lambda.cpp -o 0_lambda.exe; ./0_lambda.exe
	g++ -std=c++14 0_bind.cpp -o 0_bind.exe; ./0_bind.exe
	g++ -std=c++14 1_thread.cpp -o 1_thread.exe -lpthread; ./1_thread.exe
	g++ -std=c++14 2_switch.cpp -o 2_switch.exe; ./2_switch.exe

stl:
	g++ -std=c++14 -o stl.exe stl.cpp -g ; ./stl.exe


printf:
	g++ -std=c++14 -o 90_printf.exe 90_printf.cpp -g ; ./90_printf.exe

strstream:
	g++ -std=c++14 -o 91_strstream.exe 91_strstream.cpp -g ; ./91_strstream.exe

p2:
	g++ -std=c++17 -o 92_temp.exe 92_temp.cpp -g ; ./92_temp.exe

p3:
	g++ -std=c++17 -o 93_temp.exe 93_temp.cpp -g ; ./93_temp.exe

const:
	g++ -std=c++14 30_const.cpp -o 30_const.exe; ./30_const.exe

o:
	g++ -g -std=c++14 14_o.cpp -o 14_o.exe -lpthread; ./14_o.exe

m:
	g++ -g -std=c++14 60_move.cpp -o 60_move.exe -lpthread; ./60_move.exe

m2:
	g++ -g -std=c++14 62_temp.cpp -o 62_temp.exe -lpthread; ./62_temp.exe

map17:
	g++-7 -g -std=c++17 50_map.cpp -o 50_map.exe ; ./50_map.exe

bind17:
	g++-7 -g -std=c++17 51_bind.cpp -o 51_bind.exe ; ./51_bind.exe
bind172:
	g++-7 -g -std=c++17 52_bind.cpp -o 52_bind.exe ; ./52_bind.exe


doubleit:
	g++-7 -g -std=c++17 60_double_it.cpp -o 60_double_it.exe ; ./60_double_it.exe

tripple:
	g++-7 -g -std=c++17 61_tripple.cpp -o 61_tripple.exe ; ./61_tripple.exe

type:
	g++-7 -g -std=c++17 70_type.cpp -o 70_type.exe ; ./70_type.exe


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

conv:
	g++ -std=c++14 20_conv.cpp -o 20_conv.exe; ./20_conv.exe


var5:
	g++ -std=c++14 5_var_data.cpp -o 5_var_data.exe; ./5_var_data.exe

var6:
	g++ -std=c++14 6_var.cpp -o 6_var_data.exe; ./6_var_data.exe

var7:
	g++ -std=c++14 7_forward.cpp -o 7_forward.exe; ./7_forward.exe

list80:
	g++ -g -std=c++17 80_list.cpp -o 80_list.exe; ./80_list.exe

list81:
	g++ -g -std=c++17 81_list.cpp -o 81_list.exe; ./81_list.exe

mem81:
	g++ -g -std=c++14 81_mem.cpp -o 81_mem.exe; ./81_mem.exe

rtti:
	g++ -g -std=c++14 87_rtti.cpp -frtti -o 87_rtti.exe; ./87_rtti.exe

mach:
	g++ -I ../../Mach7/code/mach7 -I ../../Mach7/code -g -std=c++14 94_mach.cpp -frtti -o 94_mach.exe; ./94_mach.exe

rtti2:
	g++ -g -std=c++17 95_rtti.cpp -o 95_rtti.s -S
	g++ -g -std=c++17 95_rtti.cpp -o 95_rtti.exe; ./95_rtti.exe

r0:
	g++ -g -std=c++17 97_r0.cpp -o 97_r0.exe; ./97_r0.exe


# var8:
# 	g++ -std=c++14 8_enable.cpp -o 8_enable.exe; ./8_enable.exe


io:
	g++ 80_print.cpp -o 80_print.exe; ./80_print.exe

var5-clang:
	clang++ -std=c++14 -Xclang -fdump-record-layouts 5_var_data.cpp -o 5_var_data.exe; ./5_var_data.exe

l4.exe: l4.o l4main.c
	cc  l4.o l4main.c -o l4.exe



ptrm:
	g++ -g -std=c++17 84_ptrm.cpp -o 84_ptrm.exe; ./84_ptrm.exe

ptrm86:
	g++ -g -std=c++17 86_ptrm.cpp -o 86_ptrm.exe; ./86_ptrm.exe

mixin:
	g++ -g -std=c++17 85_mixin.cpp -o 85_mixin.exe; ./85_mixin.exe


cmp:
	clang++ -DDEFINE_B -DDEFINE_A 15_cmp.cpp -o 15_cmp.exe
#
#  { "directory": "/home/user/llvm/build",
#    "command": "/usr/bin/clang++ -Irelative -DSOMEDEF=\"With spaces, quotes and \\-es.\" -c -o file.o file.cc",
#    "file": "file.cc" },
#  ...
#]

c17-update:
	apt-get install software-properties-common
	add-apt-repository ppa:ubuntu-toolchain-r/test
	apt-get update
	apt-get install -y gcc-7
	apt-get install -y g++-7


#   sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y;
#   sudo apt-get update -qq
#   sudo apt-get install g++-4.9

cling:
	wget https://root.cern.ch/download/cling/cling_2017-11-02_ubuntu16.tar.bz2
	wget https://root.cern.ch/download/cling/cling_2017-11-13_ubuntu16.tar.bz2


cling-build-prepare:
	sudo apt install g++-6 gcc-6

cling-build:
	mkdir -p $(CURDIR)/dep/cmake
	curl -k https://cmake.org/files/v3.8/cmake-3.8.0-Linux-x86_64.tar.gz | tar --strip-components=1 -xz -C $(CURDIR)/dep/cmake
	rm -rf cling
	git clone https://github.com/root-project/cling
	export CMAKE=$(CURDIR)/dep/cmake/bin/cmake; \
	export CXX="g++-6"; \
	export CC="gcc-6"; \
	cd cling; tools/packaging/cpt.py --no-test --tarball-tag=master --with-cling-url=https://github.com/root-project/cling --with-clang-url=http://root.cern.ch/git/clang.git --with-llvm-url=http://root.cern.ch/git/llvm.git '--with-cmake-flags=-DCMAKE_CXX_STANDARD=17 -DCMAKE_CXX_STANDARD_REQUIRED=ON -DCXX_EXTENSIONS=OFF'



d100:
	g++ -g 100_del.cpp -o 100_del.exe

d101:
	g++ -g 101_del.cpp -o 101_del.exe
