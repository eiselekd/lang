
all:
	verilator --trace --cc our.v --exe sim_main.cpp
	make -j -C obj_dir -f Vour.mk Vour
	obj_dir/Vour

wave:
	gtkwave dpi.vcd
