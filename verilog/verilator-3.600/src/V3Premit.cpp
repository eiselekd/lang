// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Add temporaries, such as for premit nodes
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
// V3Premit's Transformations:
//		
// Each module:
//	For each wide OP, make a a temporary variable with the wide value
//	For each deep expression, assign expression to temporary.
//
//*************************************************************************

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <algorithm>
#include <list>

#include "V3Global.h"
#include "V3Premit.h"
#include "V3Ast.h"

//######################################################################
// Premit state, as a visitor of each AstNode

class PremitVisitor : public AstNVisitor {
private:
    // NODE STATE
    //  AstNodeMath::user()	-> bool.  True if iterated already
    //  AstShiftL::user2()	-> bool.  True if converted to conditional
    //  AstShiftR::user2()	-> bool.  True if converted to conditional

    // STATE
    AstModule*		m_modp;		// Current module
    AstCFunc*		m_funcp;	// Current block
    AstNode*		m_stmtp;	// Current statement
    AstWhile*		m_inWhilep;	// Inside while loop, special statement additions
    AstTraceInc*	m_inTracep;	// Inside while loop, special statement additions
    bool		m_assignLhs;	// Inside assignment lhs, don't breakup extracts

    //int debug() { return 9; }

    // METHODS
    bool assignNoTemp(AstNodeAssign* nodep) {
 	return (nodep->lhsp()->castVarRef()
		&& !nodep->lhsp()->castVarRef()->varp()->isSc()
		&& nodep->rhsp()->castConst());
    }
    void checkNode(AstNode* nodep) {
	// Consider adding a temp for this expression.
	// We need to avoid adding temps to the following:
	//   ASSIGN(x, *here*)
	//   ASSIGN(CONST*here*, VARREF(!sc))
	//   ARRAYSEL(*here*, ...)   (No wides can be in any argument but first, so we don't check which arg is wide)
	//   ASSIGN(x, SEL*HERE*(ARRAYSEL()...)   (m_assignLhs==true handles this.)
	if (m_stmtp
	    && !nodep->user()) {	// Not already done
	    if (nodep->isWide()) {		// Else might be cell interconnect or something
		if (m_assignLhs) {
		} else if (nodep->backp()->castNodeAssign()
			   && assignNoTemp(nodep->backp()->castNodeAssign())) {
		    // Not much point if it's just a direct assignment to a constant
		} else if (nodep->backp()->castArraySel()) {  // ArraySel's are pointer refs, ignore
		} else {
		    UINFO(4,"Cre Temp: "<<nodep<<endl);
		    createDeepTemp(nodep);
		}
	    }
	}
    }

    AstVar* getBlockTemp(AstNode* nodep) {
	string newvarname = ((string)"__Vtemp__"+cvtToStr(m_modp->varNumGetInc()));
	AstVar* varp = new AstVar (nodep->fileline(), AstVarType::STMTTEMP, newvarname,
				   new AstRange(nodep->fileline(), nodep->widthMin()-1, 0));
	m_funcp->addInitsp(varp);
	return varp;
    }

    void insertBeforeStmt(AstNode* newp) {
	// Insert newp before m_stmtp
	if (m_inWhilep) {
	    // Statements that are needed for the 'condition' in a while actually have to
	    // be put before & after the loop, since we can't do any statements in a while's (cond).
	    m_inWhilep->addPrecondsp(newp);
	} else if (m_inTracep) {
	    m_inTracep->addPrecondsp(newp);
	} else {
	    AstNRelinker linker;
	    m_stmtp->unlinkFrBack(&linker);
	    newp->addNext(m_stmtp);
	    linker.relink(newp);
	}
    }

    void createDeepTemp(AstNode* nodep) {
	if (debug()>8) nodep->dumpTree(cout,"deepin:");

	AstNRelinker linker;
	nodep->unlinkFrBack(&linker);

	AstVar* varp = getBlockTemp(nodep);
	// Replace node tree with reference to var
	AstVarRef* newp = new AstVarRef (nodep->fileline(), varp, false);
	linker.relink(newp);
	// Put assignment before the referencing statement
	AstAssign* assp = new AstAssign (nodep->fileline(),
					 new AstVarRef(nodep->fileline(), varp, true),
					 nodep);
	insertBeforeStmt(assp);
	if (debug()>8) assp->dumpTree(cout,"deepou:");
	nodep->user(true);  // Don't add another assignment
    }

    // VISITORS
    virtual void visit(AstModule* nodep, AstNUser*) {
	UINFO(4," MOD   "<<nodep<<endl);
	m_modp = nodep;
	m_funcp = NULL;
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstCFunc* nodep, AstNUser*) {
	m_funcp = nodep;
	nodep->iterateChildren(*this);
    }
    void startStatement(AstNode* nodep) {
	m_assignLhs = false;
	if (m_funcp) m_stmtp = nodep;
    }
    virtual void visit(AstWhile* nodep, AstNUser*) {
	UINFO(4,"  WHILE  "<<nodep<<endl);
	startStatement(nodep);
	nodep->precondsp()->iterateAndNext(*this);
	startStatement(nodep);
	m_inWhilep = nodep;
	nodep->condp()->iterateAndNext(*this);
	m_inWhilep = NULL;
	startStatement(nodep);
	nodep->bodysp()->iterateAndNext(*this);
	m_stmtp = NULL;
    }
    virtual void visit(AstNodeAssign* nodep, AstNUser*) {
	startStatement(nodep);
	nodep->rhsp()->iterateAndNext(*this);
	m_assignLhs = true;
	nodep->lhsp()->iterateAndNext(*this);
	m_assignLhs = false;
	m_stmtp = NULL;
    }
    virtual void visit(AstNodeStmt* nodep, AstNUser*) {
	UINFO(4,"  STMT  "<<nodep<<endl);
	startStatement(nodep);
	nodep->iterateChildren(*this);
	m_stmtp = NULL;
    }
    virtual void visit(AstTraceInc* nodep, AstNUser*) {
	startStatement(nodep);
	m_inTracep = nodep;
	nodep->iterateChildren(*this);
	m_inTracep = NULL;
	m_stmtp = NULL;
    }
    void visitShift (AstNodeBiop* nodep) {
	// Shifts of > 32/64 bits in C++ will wrap-around and generate non-0s
	if (!nodep->user2()) {
	    UINFO(4,"  ShiftFix  "<<nodep<<endl);
	    nodep->user2(true);
	    if (nodep->widthMin()<=64  // Else we'll use large operators which work right
		// C operator's width must be < maximum shift which is based on Verilog width
		&& nodep->width() < (1LL<<nodep->rhsp()->widthMin())) {
		AstNRelinker replaceHandle;
		nodep->unlinkFrBack(&replaceHandle);
		AstNode* constzerop;
		if (nodep->signedFlavor()) {
		    // Then over shifting gives the sign bit, not all zeros
		    // Note *NOT* clean output -- just like normal shift!
		    // Create equivalent of VL_SIGNONES_(node_width)
		    constzerop = new AstUnaryMin (nodep->fileline(),
						  new AstShiftR(nodep->fileline(),
								nodep->lhsp()->cloneTree(false),
								new AstConst(nodep->fileline(),
									     nodep->widthMin()-1),
								nodep->width()));
		} else {
		    V3Number zeronum  (nodep->fileline(), nodep->width(), 0);
		    constzerop = new AstConst(nodep->fileline(), zeronum);
		}
		constzerop->widthFrom (nodep);  // unsigned
		V3Number widthnum (nodep->fileline(), nodep->rhsp()->widthMin(), nodep->width()-1);
		AstNode* constwidthp = new AstConst(nodep->fileline(), widthnum);
		constwidthp->widthFrom (nodep->rhsp());  // unsigned
		AstCond* newp =
		    new AstCond (nodep->fileline(),
				 new AstLte (nodep->fileline(),
					     nodep->rhsp()->cloneTree(false),
					     constwidthp),
				 nodep,
				 constzerop);
		newp->widthSignedFrom(nodep);
		replaceHandle.relink(newp);
	    }
	}
	nodep->iterateChildren(*this); checkNode(nodep);
    }
    virtual void visit(AstShiftL* nodep, AstNUser*) {
	visitShift(nodep);
    }
    virtual void visit(AstShiftR* nodep, AstNUser*) {
	visitShift(nodep);
    }
    virtual void visit(AstShiftRS* nodep, AstNUser*) {
	visitShift(nodep);
    }
    // Operators
    virtual void visit(AstNodeTermop* nodep, AstNUser*) {
	nodep->iterateChildren(*this); checkNode(nodep); }
    virtual void visit(AstNodeUniop* nodep, AstNUser*) {
	nodep->iterateChildren(*this); checkNode(nodep); }
    virtual void visit(AstNodeBiop* nodep, AstNUser*) {
	nodep->iterateChildren(*this); checkNode(nodep); }
    virtual void visit(AstSel* nodep, AstNUser*) {
	nodep->fromp()->iterateAndNext(*this);
	{   // Only the 'from' is part of the assignment LHS
	    bool prevAssign = m_assignLhs;
	    m_assignLhs = false;
	    nodep->lsbp()->iterateAndNext(*this);
	    nodep->widthp()->iterateAndNext(*this);
	    m_assignLhs = prevAssign;
	}
	checkNode(nodep); }
    virtual void visit(AstConst* nodep, AstNUser*) {
	nodep->iterateChildren(*this); checkNode(nodep); }
    virtual void visit(AstNodeCond* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	if (nodep->expr1p()->isWide()
	    && !nodep->condp()->castConst()
	    && !nodep->condp()->castVarRef()) {
	    // We're going to need the expression several times in the expanded code,
	    // so might as well make it a common expression
	    createDeepTemp(nodep->condp());
	}
	checkNode(nodep);
    }

    //--------------------
    // Default: Just iterate
    virtual void visit(AstVar* nodep, AstNUser*) {}	// Don't hit varrefs under vars
    virtual void visit(AstNode* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
    }

public:
    // CONSTUCTORS
    PremitVisitor() {
	m_modp = NULL;
	m_funcp = NULL;
	m_stmtp = NULL;
	m_inWhilep = NULL;
	m_inTracep = NULL;
    }
    virtual ~PremitVisitor() {}
    void main(AstNode* nodep) {
	AstNode::userClearTree();	// userp() used on entire tree
	AstNode::user2ClearTree();	// user2p() used on entire tree
	nodep->accept(*this);
    }
};

//----------------------------------------------------------------------
// Top loop

//######################################################################
// Premit class functions

void V3Premit::premitAll(AstNetlist* nodep) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    PremitVisitor visitor;
    visitor.main(nodep);
}
