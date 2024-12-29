// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Add temporaries, such as for changed nodes
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
// V3Changed's Transformations:
//		
// Each module:
//	Each combo block
//	    For each variable that comes from combo block and is generated AFTER a usage
//		Add __Vlast_{var} to local section, init to current value (just use array?)
//		Change = if any var != last.
//	    If a signal is used as a clock in this module or any
//	    module *below*, and it isn't a input to this module,
//	    we need to indicate a new clock has been created.
//
//*************************************************************************

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <algorithm>
#include <set>

#include "V3Global.h"
#include "V3Ast.h"
#include "V3Changed.h"
#include "V3EmitCBase.h"

//######################################################################
// Changed state, as a visitor of each AstNode

class ChangedVisitor : public AstNVisitor {
private:
    // NODE STATE
    // Entire netlist:
    //  AstVarScope::user()		-> bool.  True indicates processed
    // STATE
    AstModule*		m_topModp;	// Top module
    AstScope*		m_scopetopp;	// Scope under TOPSCOPE
    AstCFunc*		m_chgFuncp;	// Change function we're building
    //int debug() { return 9; }

    // METHODS
    void genChangeDet(AstVarScope* vscp) {
#ifdef NEW_ORDERING
	vscp->v3fatalSrc("Not applicable\n");
#endif
	AstVar* varp = vscp->varp();
	if (varp->arraysp()) {
	    vscp->v3error("Unsupported: Can't detect changes on arrayed variable (probably with UNOPTFLAT warning suppressed): "<<varp->prettyName());
	} else {
	    string newvarname = "__Vchglast__"+vscp->scopep()->nameDotless()+"__"+varp->shortName();
	    // Create:  VARREF(_last)
	    //          ASSIGN(VARREF(_last), VARREF(var))
	    //          ...
	    //          CHANGEDET(VARREF(_last), VARREF(var))
	    AstVar* newvarp = new AstVar (varp->fileline(), AstVarType::MODULETEMP, newvarname, varp);
	    m_topModp->addStmtp(newvarp);
	    AstVarScope* newvscp = new AstVarScope(vscp->fileline(), m_scopetopp, newvarp);
	    m_scopetopp->addVarp(newvscp);
	    AstChangeDet* changep
		= new AstChangeDet (vscp->fileline(),
				    new AstVarRef(vscp->fileline(), vscp, false),
				    new AstVarRef(vscp->fileline(), newvscp, false),
				    false);
	    m_chgFuncp->addStmtsp(changep);
	    AstAssign* initp
		= new AstAssign (vscp->fileline(),
				 new AstVarRef(vscp->fileline(), newvscp, true),
				 new AstVarRef(vscp->fileline(), vscp, false));
	    m_chgFuncp->addFinalsp(initp);
	}
    }

    // VISITORS
    virtual void visit(AstModule* nodep, AstNUser*) {
	UINFO(4," MOD   "<<nodep<<endl);
	if (nodep->isTop()) {
	    m_topModp = nodep;
	}
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstTopScope* nodep, AstNUser*) {
	UINFO(4," TS "<<nodep<<endl);
	// Clearing
	AstNode::userClearTree();
	// Create the change detection function
	AstScope* scopep = nodep->scopep();
	if (!scopep) nodep->v3fatalSrc("No scope found on top level, perhaps you have no statements?\n");
	m_scopetopp = scopep;
	// Create change detection function
	m_chgFuncp = new AstCFunc(nodep->fileline(), "_change_request", scopep, "bool");
	m_chgFuncp->argTypes(EmitCBaseVisitor::symClassVar());
	m_chgFuncp->symProlog(true);
	m_chgFuncp->declPrivate(true);
	m_scopetopp->addActivep(m_chgFuncp);
	// We need at least one change detect so we know to emit the correct code
	m_chgFuncp->addStmtsp(new AstChangeDet(nodep->fileline(), NULL, NULL, false));
	//
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstVarScope* nodep, AstNUser*) {
	if (nodep->isCircular()) {
	    UINFO(8,"  CIRC "<<nodep<<endl);
	    if (!nodep->user()) {
		nodep->user(true);
		genChangeDet(nodep);
	    }
	}
    }
    //--------------------
    // Default: Just iterate
    virtual void visit(AstNode* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
    }

public:
    // CONSTUCTORS
    ChangedVisitor(AstNetlist* nodep) {
	m_topModp = NULL;
	m_chgFuncp = NULL;
	m_scopetopp = NULL;
	nodep->accept(*this);
    }
    virtual ~ChangedVisitor() {}
};

//######################################################################
// Changed class functions

void V3Changed::changedAll(AstNetlist* nodep) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    ChangedVisitor visitor (nodep);
}
