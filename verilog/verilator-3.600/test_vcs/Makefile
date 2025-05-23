# $Id:$ */
#*****************************************************************************
#
# DESCRIPTION: Verilator Example: Makefile for inside source directory
#
# This calls the object directory makefile.  That allows the objects to
# be placed in the "current directory" which simplifies the Makefile.
#
# Copyright 2003 by Wilson Snyder. This program is free software; you can
# redistribute it and/or modify it under the terms of either the GNU
# General Public License or the Perl Artistic License.
#
#****************************************************************************/

# This must point to the root of the VERILATOR kit
VERILATOR_ROOT := $(shell pwd)/..
export VERILATOR_ROOT
PWD := $(shell pwd)

######################################################################

V_FILES := $(wildcard *.v ../test_v/*.v)

######################################################################
default: test

ifneq ($(VCS_HOME),)
test::	vcs
else
test::	novcs
endif

ifneq ($(NC_ROOT),)
test::	nc
else
test::	nonc
endif

######################################################################

novcs:
	@echo "No VCS simulator installed."
	@echo "Not running VCS regression test."

vcs:	vcs_passed.log

simv:	$(V_FILES) ../test_v/input.vc
	vcs +cli -I +define+vcs+1 -f ../test_v/input.vc -q bench.v

vcs_passed.log	: simv
	-rm -f test_passed.log
	./simv -l sim.log
	grep -q Finished sim.log && grep Finished sim.log > vcs_passed.log

######################################################################

nonc:
	@echo "No NC-Verilog simulator installed."
	@echo "Not running NC-Verilog regression test."

nc:	nc_passed.log

nc_passed.log: $(V_FILES) ../test_v/input.vc
	ncverilog +define+ncverilog=1 +licqueue -f ../test_v/input.vc -q bench.v
	-rm -f nc_passed.log
	grep -q Finished ncverilog.log && grep Finished ncverilog.log > nc_passed.log

######################################################################

maintainer-copy::
clean mostlyclean distclean maintainer-clean::
	-rm -rf obj_dir *.log *.dmp *.vpd simv* vcs.key csrc INCA_libs

