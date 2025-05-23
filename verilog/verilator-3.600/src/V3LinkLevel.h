// $Id$ //-*- C++ -*-
//*************************************************************************
// DESCRIPTION: Verilator: Link modules/signals together
//
// Code available from: http://www.veripool.com/verilator
//
// AUTHORS: Wilson Snyder with Paul Wasson, Duane Gabli
//
//*************************************************************************
//
// Copyright 2003-2006 by Wilson Snyder.  This program is free software; you can
// redistribute it and/or modify it under the terms of either the GNU
// General Public License or the Perl Artistic License.
//
// Verilator is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
//*************************************************************************

#ifndef _V3LINKLEVEL_H_
#define _V3LINKLEVEL_H_ 1
#include "config.h"
#include "V3Error.h"
#include "V3Ast.h"

//============================================================================

class V3LinkLevel {
public:
    static void modSortByLevel();
    static void wrapTop(AstNetlist* nodep);
};

#endif // Guard
