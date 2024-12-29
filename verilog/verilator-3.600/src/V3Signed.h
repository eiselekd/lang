// $Id$ //-*- C++ -*-
//*************************************************************************
// DESCRIPTION: Verilator: Signed/unsigned resolution
//
// Code available from: http://www.veripool.com/verilator
//
// AUTHORS: Wilson Snyder with Paul Wasson, Duane Gabli
//
//*************************************************************************
//
// Copyright 2005-2006 by Wilson Snyder.  This program is free software; you can
// redistribute it and/or modify it under the terms of either the GNU
// General Public License or the Perl Artistic License.
//
// Verilator is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
//*************************************************************************

#ifndef _V3SIGNED_H_
#define _V3SIGNED_H_ 1
#include "config.h"
#include "V3Error.h"
#include "V3Ast.h"

//============================================================================

class V3Signed {
public:
    static void signedAll(AstNetlist* nodep);
    static void signedParams(AstNode* nodep);
};

#endif // Guard
