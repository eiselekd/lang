// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Add C++ casts across expression size changes
//
// Code available from: http://www.veripool.com/verilator
//
// AUTHORS: Wilson Snyder with Paul Wasson, Duane Gabli
//
//*************************************************************************
//
// Copyright 2004-2006 by Wilson Snyder.  This program is free software; you can
// redistribute it and/or modify it under the terms of either the GNU
// General Public License or the Perl Artistic License.
//
// Verilator is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
//*************************************************************************
// V3Cast's Transformations:
//		
// Each module:
//	For each math operator, if above operator requires 32 bits,
//	and this isn't, cast to 32 bits.
//	Likewise for 64 bit operators.
//
// C++ rules:
//	Integral promotions allow conversion to larger int.  Unsigned is only
//	used if a int would not fit the value.
//
//	Bools converts to int, not unsigned.
//
//	Most operations return unsigned if either operand is unsigned.
//
//	Unsignedness can be lost on results of the below operations, as they
//	may need the sign bit for proper operation:
//		/, %, /=, %=, <, <=, >, or >=
//
//	Signed values are always sign extended on promotion or right shift,
//	even if assigning to a unsigned.
//
//*************************************************************************

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <algorithm>

#include "V3Global.h"
#include "V3Cast.h"
#include "V3Ast.h"

//######################################################################
// Cast state, as a visitor of each AstNode

class CastVisitor : public AstNVisitor {
private:
    // NODE STATE
    // Entire netlist:

    // STATE
    //int debug() { return 9; }

    // METHODS
    void insertCast(AstNode* nodep, int needsize) {  // We'll insert ABOVE passed node
	UINFO(4,"  NeedCast "<<nodep<<endl);
	AstNRelinker relinkHandle;
	nodep->unlinkFrBack(&relinkHandle);
	//
	AstCast* castp = new AstCast (nodep->fileline(), nodep, needsize);
	castp->width(needsize, nodep->widthMin());
	relinkHandle.relink(castp);
	//if (debug()>8) castp->dumpTree(cout,"-castins: ");
	//
	insureLower32Cast(castp);
    }
    int castSize (AstNode* nodep) {
	if (nodep->isQuad()) return VL_QUADSIZE;
	else if (nodep->width()<=8) return 8;
	else if (nodep->width()<=16) return 16;
	else return VL_WORDSIZE;
    }
    void insureCast(AstNode* nodep) {
	if (castSize(nodep->backp()) != castSize(nodep)) {
	    insertCast(nodep, castSize(nodep->backp()));
	}
    }
    void insureLower32Cast(AstCast* nodep) {
	// If we have uint64 = CAST(uint64(x)) then the upcasting
	// really needs to be CAST(uint64(CAST(uint32(x))).
	// Otherwise a (uint64)(a>b) would return wrong value, as
	// less than has undeterministic signedness.
	if (nodep->isQuad() && !nodep->lhsp()->isQuad()
	    && !nodep->lhsp()->castCast()) {
	    insertCast(nodep->lhsp(), VL_WORDSIZE);
	}
    }

    // VISITORS
    virtual void visit(AstNodeUniop* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	if (nodep->sizeMattersLhs()) insureCast(nodep->lhsp());
    }
    virtual void visit(AstNodeBiop* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	if (nodep->sizeMattersLhs()) insureCast(nodep->lhsp());
	if (nodep->sizeMattersRhs()) insureCast(nodep->rhsp());
    }
    virtual void visit(AstNodeTriop* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	if (nodep->sizeMattersLhs()) insureCast(nodep->lhsp());
	if (nodep->sizeMattersRhs()) insureCast(nodep->rhsp());
	if (nodep->sizeMattersThs()) insureCast(nodep->thsp());
    }
    virtual void visit(AstCast* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	insureLower32Cast(nodep);
    }
    virtual void visit(AstUnaryMin* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	if (nodep->lhsp()->widthMin()==1) {
	    // We want to avoid a GCC "converting of negative value" warning
	    // from our expansion of
	    //    out = {32{a<b}}  =>   out = - (a<b)
	    insertCast(nodep->lhsp(), castSize(nodep));
	} else {
	    insureCast(nodep->lhsp());
	}
    }
    virtual void visit(AstVarRef* nodep, AstNUser*) {
	if (!nodep->lvalue()
	    && !nodep->backp()->castCast()
	    && nodep->backp()->castNodeMath()
	    && nodep->backp()->width()
	    && castSize(nodep) != castSize(nodep->varp())) {
	    // Cast vars to IData first, else below has upper bits wrongly set
	    //  CData x=3;  out = (QData)(x<<30); 
	    insertCast (nodep, castSize(nodep));
	}
    }

    // NOPs
    virtual void visit(AstVar* nodep, AstNUser*) {}

    //--------------------
    // Default: Just iterate
    virtual void visit(AstNode* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
    }

public:
    // CONSTUCTORS
    CastVisitor(AstNetlist* nodep) {
	nodep->accept(*this);
    }
    virtual ~CastVisitor() {}
};

//######################################################################
// Cast class functions

void V3Cast::castAll(AstNetlist* nodep) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    CastVisitor visitor (nodep);
}
