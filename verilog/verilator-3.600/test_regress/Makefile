# $Id$ */
#*****************************************************************************
#
# DESCRIPTION: Verilator Example: Makefile for inside source directory
#
# This calls the object directory makefile.  That allows the objects to
# be placed in the "current directory" which simplifies the Makefile.
#
# Copyright 2003-2005 by Wilson Snyder. This program is free software; you can
# redistribute it and/or modify it under the terms of either the GNU
# General Public License or the Perl Artistic License.
#
#****************************************************************************/

# This must point to the root of the VERILATOR kit
VERILATOR_ROOT := $(shell pwd)/..
export VERILATOR_ROOT
PERL = perl
PWD := $(shell pwd)

######################################################################

default: test

ifneq ($(VCS_HOME),)
PRODUCTS += --vcs
endif

ifneq ($(NC_ROOT),)
PRODUCTS += --nc
endif

PRODUCTS += --v3

ifeq ($(VERILATOR_AUTHOR_SITE),1)
 DRIVER_FLAGS += -j 4
endif

######################################################################

test:
	$(PERL) driver.pl $(DRIVER_FLAGS) $(PRODUCTS)

######################################################################

vcs:
	$(PERL) driver.pl $(DRIVER_FLAGS) --vcs --stop

######################################################################

nc:
	$(PERL) driver.pl $(DRIVER_FLAGS) --nc --stop

######################################################################

v3:
	$(PERL) driver.pl $(DRIVER_FLAGS) --v3 --stop

######################################################################

random:
	$(PERL) driver.pl $(DRIVER_FLAGS) --optimize : --stop

random_forever:
	while ( VERILATOR_NO_DEBUG=1 CPPFLAGS_ADD=-Wno-error $(MAKE) random ) ; do \
		echo ; \
	done

######################################################################
maintainer-copy::
clean mostlyclean distclean maintainer-clean::
	-rm -rf obj_dir simv* simx* csrc INCA_libs *.log logs

