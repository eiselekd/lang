// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Branch prediction
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
// BRANCH TRANSFORMATIONS:
//	At each IF/(IF else).
//	   Count underneath $display/$stop statements.
//	   If more on if then else, this branch is unlikely, or vice-versa.
//
//*************************************************************************

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <map>

#include "V3Global.h"
#include "V3Branch.h"
#include "V3Ast.h"

//######################################################################
// Branch state, as a visitor of each AstNode

class BranchVisitor : public AstNVisitor {
private:
    // STATE
    int		m_likely;	// Excuses for branch likely taken
    int		m_unlikely;	// Excuses for branch likely not taken
    //int debug() { return 9; }

    // METHODS
    void reset() {
	m_likely = false;
	m_unlikely = false;
    }

    // VISITORS
    virtual void visit(AstNodeIf* nodep, AstNUser*) {
	UINFO(4," IF: "<<nodep<<endl);
	int lastLikely = m_likely;
	int lastUnlikely = m_unlikely;
	{
	    // Do if
	    reset();
	    nodep->ifsp()->iterateAndNext(*this);
	    int ifLikely = m_likely;
	    int ifUnlikely = m_unlikely;
	    // Do else
	    reset();
	    nodep->elsesp()->iterateAndNext(*this);
	    int elseLikely = m_likely;
	    int elseUnlikely = m_unlikely;
	    // Compute
	    int likeness = ifLikely - ifUnlikely - (elseLikely - elseUnlikely);
	    if (likeness>0) {
		nodep->branchPred(AstBranchPred::LIKELY);
	    } else if (likeness<0) {
		nodep->branchPred(AstBranchPred::UNLIKELY);
	    } // else leave unknown
	}
	m_likely = lastLikely;
	m_unlikely = lastUnlikely;
    }
    virtual void visit(AstNode* nodep, AstNUser*) {
	// Default: Just iterate
	if (nodep->isUnlikely()) {
	    UINFO(4,"  UNLIKELY: "<<nodep<<endl);
	    m_unlikely++;
	}
	nodep->iterateChildren(*this);
    }

public:
    // CONSTUCTORS
    BranchVisitor(AstNetlist* rootp) {
	reset();
	rootp->iterateChildren(*this);
    }
    virtual ~BranchVisitor() {}
};

//######################################################################
// Branch class functions

void V3Branch::branchAll(AstNetlist* rootp) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    BranchVisitor visitor (rootp);
}
