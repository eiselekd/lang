# $Id$ */
#*****************************************************************************
#
# DESCRIPTION: Verilator Example: Makefile for inside source directory
#
# This calls the object directory makefile.  That allows the objects to
# be placed in the "current directory" which simplifies the Makefile.
#
# Copyright 2003-2006 by Wilson Snyder. This program is free software; you can
# redistribute it and/or modify it under the terms of either the GNU
# General Public License or the Perl Artistic License.
#
#****************************************************************************/

# This must point to the root of the VERILATOR kit
VERILATOR_ROOT := $(shell pwd)/..
export VERILATOR_ROOT
PWD := $(shell pwd)

DEBUG_ON = --debug --trace-dups --output-split 100

######################################################################
default:  prep        preproc compile     run coverage
debug:    prep_dbg    preproc compile_dbg run coverage
nopublic: prep_dbg_np preproc compile_dbg run coverage

V_FLAGS = -f $(PWD)/../test_v/input.vc
VERILATOR_FLAGS = --public --sp --coverage --stats --trace $(V_FLAGS) top.v

prep:
	perl $(VERILATOR_ROOT)/bin/verilator             $(VERILATOR_FLAGS)
prep_dbg:
	perl $(VERILATOR_ROOT)/bin/verilator $(DEBUG_ON) $(VERILATOR_FLAGS)
prep_dbg_np:
	perl $(VERILATOR_ROOT)/bin/verilator $(DEBUG_ON) $(VERILATOR_FLAGS) --nopublic

preproc:
	cd obj_dir ; $(MAKE) -j 1 -f ../Makefile_obj preproc

compile:
	cd obj_dir ; $(MAKE)        -j 3 -f ../Makefile_obj

compile_dbg:
	cd obj_dir ; $(MAKE) OPT=-g -j 3 -f ../Makefile_obj

run:	
	obj_dir/simx

coverage:
	vcoverage $(V_FLAGS)

######################################################################

obj_dir:
	mkdir $@

######################################################################

maintainer-copy::
clean mostlyclean distclean maintainer-clean::
	-rm -rf obj_dir *.log *.dmp *.vpd *.vcd core

