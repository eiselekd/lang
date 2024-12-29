// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Add temporaries, such as for delayed nodes
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
// V3Delayed's Transformations:
//		
// Each module:
//	Replace ASSIGNDLY var, exp 
//	    With   ASSIGNDLY newvar, exp
//	    At top of block:  VAR  newvar
//	    At bottom of block: ASSIGNW var newvar
//		Need _x_dly = x at top of active if "x" is not always set
//			For now we'll say it's set if at top of block (not under IF, etc)
//		Need x = _x_dly at bottom of active if "x" is never referenced on LHS
//			in the active, and above rule applies too.  (If so, use x on LHS, not _x_dly.)
//
//	If a signal is set in multiple always blocks, we need a dly read & set with
//	multiple clock sensitivities.  We have 3 options:
//	    1. When detected, make a new ACTIVE and move earlier created delayed assignment there
//	    2. Form unique ACTIVE for every multiple clocked assignment
//	    3. Predetect signals from multiple always blocks and do #2 on them
//	    Since all 3 require a top activation cleanup, we do #2 which is easiest.
//
// ASSIGNDLY (BITSEL(ARRAYSEL (VARREF(v), bits), selbits), rhs)
// ->	VAR __Vdlyvset_x
// 	VAR __Vdlyvval_x
// 	VAR __Vdlyvdim_x
// 	VAR __Vdlyvlsb_x
//	ASSIGNW (__Vdlyvset_x,0)
//	...
//	ASSIGNW (VARREF(__Vdlyvval_x), rhs)
//	ASSIGNW (__Vdlyvdim_x, dimension_number)
//	ASSIGNW (__Vdlyvset_x, 1)
//	...
//	ASSIGNW (BITSEL(ARRAYSEL(VARREF(x), __Vdlyvdim_x), __Vdlyvlsb_x), __Vdlyvval_x)
//
//*************************************************************************

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <algorithm>
#include <map>
#include <deque>

#include "V3Global.h"
#include "V3Delayed.h"
#include "V3Ast.h"
#include "V3Stats.h"

//######################################################################
// Delayed state, as a visitor of each AstNode

class DelayedVisitor : public AstNVisitor {
private:
    // NODE STATE
    // Cleared each module:
    //  AstVarScope::userp()	-> AstVarScope*.  Points to temp var created.
    //  AstVarScope::user2p()	-> AstActive*.  Points to activity block of signal
    //  AstVarScope::user4p()	-> AstAlwaysPost*.  Post block for this variable
    //  AstVar::user2()		-> bool.  Set true if already made warning
    //  AstVar::user3()		-> VarUsage. Tracks delayed vs non-delayed usage
    //  AstVar::user4()		-> int.   Vector number, for assignment creation
    //  AstVarRef::user2()	-> bool.  Set true if already processed
    //  AstAlwaysPost::user4()	-> AstIf*.  Last IF (__Vdlyvset__) created under this AlwaysPost
    // Cleared each scope:
    //  AstAssignDly::user5()	-> AstVarScope*.  __Vdlyvset__ created for this assign
    //  AstAlwaysPost::user5()	-> AstVarScope*.  __Vdlyvset__ last referenced in IF

    enum VarUsage { VU_NONE=0, VU_DLY=1, VU_NONDLY=2 };

    // STATE
    AstActive*		m_activep;	// Current activate
    AstCFunc*		m_cfuncp;	// Current public C Function
    AstAssignDly*	m_nextDlyp;	// Next delayed assignment in a list of assignments
    bool		m_inDly;	// True in delayed assignments
    bool		m_inLoop;	// True in for loops
    bool		m_inInitial;	// True in intial blocks
    typedef std::map<pair<AstModule*,string>,AstVar*> VarMap;
    VarMap		m_modVarMap;	// Table of new var names created under module
    V3Double0		m_statSharedSet;// Statistic tracking

    //static int debug() { return 9; }

    // METHODS
    void markVarUsage(AstVar* nodep, uint32_t flags) {
	//UINFO(4," MVU "<<flags<<" "<<nodep<<endl);
	nodep->user3( nodep->user3() | flags );
	if ((nodep->user3() & VU_DLY) && (nodep->user3() & VU_NONDLY)) {
	    nodep->v3warn(BLKANDNBLK,"Unsupported: Blocked and non-blocking assignments to same variable: "<<nodep->prettyName());
	}
    }
    AstVarScope* createVarSc(AstVarScope* oldvarscp, string name, int width/*0==fromoldvar*/) {
	// Because we've already scoped it, we may need to add both the AstVar and the AstVarScope
	AstRange* rangep = NULL;
	if (width==0) {
	    rangep = new AstRange(oldvarscp->fileline(),
				  oldvarscp->varp()->msb(),
				  oldvarscp->varp()->lsb());
	} else if (width==1) {
	    rangep = NULL;
	} else {
	    rangep = new AstRange(oldvarscp->fileline(),
				  width-1, 0);
	}

	if (!oldvarscp->scopep()) oldvarscp->v3fatalSrc("Var unscoped");
	AstVar* varp;
	AstModule* addmodp = oldvarscp->scopep()->modp();
	// We need a new AstVar, but only one for all scopes, to match the new AstVarScope
	VarMap::iterator iter = m_modVarMap.find(make_pair(addmodp,name));
	if (iter != m_modVarMap.end()) {
	    // Created module's AstVar earlier under some other scope
	    varp = iter->second;
	} else {
	    varp = new AstVar (oldvarscp->fileline(), AstVarType::BLOCKTEMP, name, rangep);
	    if (width==0) varp->widthSignedFrom(oldvarscp);
	    addmodp->addStmtp(varp);
	    m_modVarMap.insert(make_pair(make_pair(addmodp, name), varp));
	}

	AstVarScope* varscp = new AstVarScope (oldvarscp->fileline(), oldvarscp->scopep(), varp);
	oldvarscp->scopep()->addVarp(varscp);
	return varscp;
    }

    AstNode* createDlyArray(AstAssignDly* nodep, AstNode* lhsp) {
	// Create delayed assignment
	// See top of this file for transformation
	// Return the new LHS for the assignment, Null = unlink
	// Find selects
	AstNode* newlhsp = NULL;	// NULL = unlink old assign
	AstSel*  bitselp = NULL;
	AstArraySel*  arrayselp = NULL;
	if (lhsp->castSel()) {
	    bitselp = lhsp->castSel();
	    arrayselp = bitselp->fromp()->castArraySel();
	} else {
	    arrayselp = lhsp->castArraySel();
	}
	if (!arrayselp) nodep->v3fatalSrc("No arraysel under bitsel?");

	UINFO(4,"AssignDlyArray: "<<nodep<<endl);
	//
	//=== Dimensions: __Vdlyvdim__
	deque<AstNode*> dimvalp;		// Assignment value for each dimension of assignment
	AstNode* dimselp=arrayselp;
	for (; dimselp->castArraySel(); dimselp=dimselp->castArraySel()->fromp()) {
	    AstNode* valp = dimselp->castArraySel()->bitp()->unlinkFrBack();
	    dimvalp.push_front(valp);
	}
	AstVarRef* varrefp = dimselp->castVarRef();
	if (!varrefp) nodep->v3fatalSrc("No var underneath arraysels\n");
	if (!varrefp->varScopep()) varrefp->v3fatalSrc("Var didn't get varscoped in V3Scope.cpp\n");
	varrefp->unlinkFrBack();
	AstVar* oldvarp = varrefp->varp();
	int modVecNum = oldvarp->user4();  oldvarp->user4(modVecNum+1);
	//
	deque<AstNode*> dimreadps;		// Read value for each dimension of assignment
	for (unsigned dimension=0; dimension<dimvalp.size(); dimension++) {
	    AstNode* dimp = dimvalp[dimension];
	    if (dimp->castConst()) { // bit = const, can just use it
		dimreadps.push_front(dimp);
	    } else {
		string bitvarname = (string("__Vdlyvdim")+cvtToStr(dimension)
				     +"__"+oldvarp->shortName()+"__"+cvtToStr(modVecNum));
		AstVarScope* bitvscp = createVarSc(varrefp->varScopep(), bitvarname, dimp->width());
		AstAssign* bitassignp
		    = new AstAssign (nodep->fileline(),
				     new AstVarRef(nodep->fileline(), bitvscp, true),
				     dimp);
		nodep->addNextHere(bitassignp);
		dimreadps.push_front(new AstVarRef(nodep->fileline(), bitvscp, false));
	    }
	}
	//
	//=== Bitselect: __Vdlyvlsb__
	AstNode* bitreadp=NULL;  // Code to read Vdlyvlsb
	if (bitselp) {
	    AstNode* lsbvaluep = bitselp->lsbp()->unlinkFrBack();
	    if (bitselp->fromp()->castConst()) {// vlsb = constant, can just push constant into where we use it
		bitreadp = lsbvaluep;
	    } else {
		string bitvarname = (string("__Vdlyvlsb__")+oldvarp->shortName()+"__"+cvtToStr(modVecNum));
		AstVarScope* bitvscp = createVarSc(varrefp->varScopep(), bitvarname, lsbvaluep->width());
		AstAssign* bitassignp = new AstAssign (nodep->fileline(),
						       new AstVarRef(nodep->fileline(), bitvscp, true),
						       lsbvaluep);
		nodep->addNextHere(bitassignp);
		bitreadp = new AstVarRef(nodep->fileline(), bitvscp, false);
	    }
	}
	//
	//=== Value: __Vdlyvval__
	AstNode* valreadp;	// Code to read Vdlyvval
	if (nodep->rhsp()->castConst()) {	// vval = constant, can just push constant into where we use it
	    valreadp = nodep->rhsp()->unlinkFrBack();
	} else {
	    string valvarname = (string("__Vdlyvval__")+oldvarp->shortName()+"__"+cvtToStr(modVecNum));
	    AstVarScope* valvscp = createVarSc(varrefp->varScopep(), valvarname, nodep->rhsp()->width());
	    newlhsp = new AstVarRef(nodep->fileline(), valvscp, true);
	    valreadp = new AstVarRef(nodep->fileline(), valvscp, false);
	}
	//
	//=== Setting/not setting boolean: __Vdlyvset__
	bool sharedVset = false;
	AstVarScope* setvscp;

	if (nodep->user5p()) {
	    // Simplistic optimization.  If the previous statement in same scope was also a =>,
	    // then we told this nodep->user5 we can use its Vdlyvset rather then making a new one.
	    // This is good for code like:
	    //    for (i=0; i<5; i++)  vector[i] <= something;
	    sharedVset = true;
	    setvscp = nodep->user5p()->castNode()->castVarScope();
	    m_statSharedSet++;
	} else {  // Create new one
	    string setvarname = (string("__Vdlyvset__")+oldvarp->shortName()+"__"+cvtToStr(modVecNum));
	    setvscp = createVarSc(varrefp->varScopep(), setvarname, 1);
	    AstAssignPre* setinitp
		= new AstAssignPre (nodep->fileline(),
				    new AstVarRef(nodep->fileline(), setvscp, true),
				    new AstConst(nodep->fileline(), 0));
	    m_activep->addStmtsp(setinitp);
	    AstAssign* setassignp
		= new AstAssign (nodep->fileline(),
				 new AstVarRef(nodep->fileline(), setvscp, true),
				 new AstConst(nodep->fileline(),
					      V3Number(nodep->fileline(),1,true)));
	    nodep->addNextHere(setassignp);
	}
	if (m_nextDlyp) {  // Tell next assigndly it can share the variable
	    m_nextDlyp->user5p(setvscp);
	}
	//
	// Create ALWAYSPOST for delayed variable
	// We add all logic to the same block if it's for the same memory
	// This insures that multiple assignments to the same memory will result
	// in correctly ordered code - the last assignment must be last.
	// It also has the nice side effect of assisting cache locality.
	AstNode* selectsp = varrefp;
	for (int dimension=int(dimreadps.size())-1; dimension>=0; --dimension) {
	    selectsp = new AstArraySel(nodep->fileline(), selectsp, dimreadps[dimension]);
	}
	if (bitselp) {
	    selectsp = new AstSel(nodep->fileline(), selectsp, bitreadp,
				  bitselp->widthp()->cloneTree(false));
	}
	// Build "IF (changeit) ...
	UINFO(9,"   For "<<setvscp<<endl);
	UINFO(9,"     & "<<varrefp<<endl);
	AstAlwaysPost* finalp = varrefp->varScopep()->user4p()->castNode()->castAlwaysPost();
	if (!finalp) {
	    finalp = new AstAlwaysPost(nodep->fileline(), NULL/*sens*/, NULL/*body*/);
	    UINFO(9,"     Created "<<finalp<<endl); 
	    m_activep->addStmtsp(finalp);
	    varrefp->varScopep()->user4p(finalp);
	}
	AstIf* postLogicp;
	if (finalp->user5p()->castNode() == setvscp) {
	    // Optimize as above; if sharing Vdlyvset *ON SAME VARIABLE*,
	    // we can share the IF statement too
	    postLogicp = finalp->user4p()->castNode()->castIf();
	    if (!postLogicp) nodep->v3fatalSrc("Delayed assignment misoptimized; prev var found w/o associated IF");
	} else {
	    postLogicp = new AstIf (nodep->fileline(),
				    new AstVarRef(nodep->fileline(), setvscp, false),
				    NULL,
				    NULL);
	    UINFO(9,"     Created "<<postLogicp<<endl); 
	    finalp->addBodysp(postLogicp);
	    finalp->user5p(setvscp);	// Remember IF's vset variable
	    finalp->user4p(postLogicp);	// and the associated IF, as we may be able to reuse it
	}
	postLogicp->addIfsp(new AstAssign(nodep->fileline(), selectsp, valreadp));

	return newlhsp;
    }

    // VISITORS
    virtual void visit(AstNetlist* nodep, AstNUser*) {
	//VV*****  We reset all userp() on the netlist
	m_modVarMap.clear();
	AstNode::userClearTree();
	AstNode::user2ClearTree();
	AstNode::user3ClearTree();
	AstNode::user4ClearTree();
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstScope* nodep, AstNUser*) {
	UINFO(4," MOD   "<<nodep<<endl);
	AstNode::user5ClearTree();
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstCFunc* nodep, AstNUser*) {
	m_cfuncp = nodep;
	nodep->iterateChildren(*this);
	m_cfuncp = NULL;
    }
    virtual void visit(AstActive* nodep, AstNUser*) {
	m_activep = nodep;
	bool oldinit = m_inInitial;
	m_inInitial = nodep->hasInitial();
	nodep->iterateChildren(*this);
	m_inInitial = oldinit;
    }
    virtual void visit(AstAssignDly* nodep, AstNUser*) {
	m_inDly = true;
	m_nextDlyp = nodep->nextp()->castAssignDly();  // Next assignment in same block, maybe NULL.
	if (m_cfuncp) nodep->v3error("Unsupported: Delayed assignment inside public function/task");
	if (nodep->lhsp()->castArraySel()
	    || (nodep->lhsp()->castSel()
		&& nodep->lhsp()->castSel()->fromp()->castArraySel())) {
	    AstNode* lhsp = nodep->lhsp()->unlinkFrBack();
	    AstNode* newlhsp = createDlyArray(nodep, lhsp);
	    if (m_inLoop) nodep->v3error("Unsupported: Delayed assignment to array inside for loops (non-delayed is ok - see docs)");
	    if (newlhsp) {
		nodep->lhsp(newlhsp);
	    } else {
		nodep->unlinkFrBack()->deleteTree(); nodep=NULL;
	    }
	    lhsp->deleteTree(); lhsp=NULL;
	}
	else {
	    nodep->iterateChildren(*this);
	}
	m_inDly = false;
	m_nextDlyp = NULL;
    }
    virtual void visit(AstVarRef* nodep, AstNUser*) {
	if (!nodep->user2()) {  // Not done yet
	    nodep->user2(true);
	    
	    if (m_inDly && nodep->lvalue()) {
		UINFO(4,"AssignDlyVar: "<<nodep<<endl);
		markVarUsage(nodep->varp(), VU_DLY);
		if (!m_activep) nodep->v3fatalSrc("<= not under sensitivity block");
		if (!m_activep->hasClocked()) nodep->v3error("Internal: Blocking <= assignment in non-clocked block, should have converted in V3Active");
		AstVarScope* oldvscp = nodep->varScopep();
		if (!oldvscp) nodep->v3fatalSrc("Var didn't get varscoped in V3Scope.cpp\n");
		AstVarScope* dlyvscp = oldvscp->userp()->castNode()->castVarScope();
		if (dlyvscp) {  // Multiple use of delayed variable
		    AstActive* oldactivep = dlyvscp->user2p()->castNode()->castActive();
		    if (!oldactivep) nodep->v3fatalSrc("<= old dly assignment not put under sensitivity block");
		    if (oldactivep->sensesp() != m_activep->sensesp()) {
			if (!nodep->varp()->fileline()->warnIsOff(V3ErrorCode::MULTIDRIVEN)
			    && !nodep->varp()->user2()) {
			    nodep->varp()->v3warn(MULTIDRIVEN,"Signal has multiple driving blocks: "<<nodep->varp()->prettyName());
			    nodep->v3warn(MULTIDRIVEN,"... Location of first driving block");
			    oldactivep->v3warn(MULTIDRIVEN,"... Location of other driving block");
			    nodep->varp()->user2(true);
			}
			UINFO(4,"AssignDupDlyVar: "<<nodep<<endl);
			UINFO(4,"  Act: "<<m_activep<<endl);
			UINFO(4,"  Act: "<<oldactivep<<endl);
			// Make a new sensitivity list, which is the combination of both blocks
			AstSenItem* sena = m_activep->sensesp()->sensesp()->cloneTree(true)->castSenItem();
			AstSenItem* senb = oldactivep->sensesp()->sensesp()->cloneTree(true)->castSenItem();
			AstSenTree* treep = new AstSenTree(m_activep->fileline(), sena);
			if (senb) treep->addSensesp(senb);
			if (AstSenTree* storep = oldactivep->sensesStorep()) {
			    storep->unlinkFrBack();
			    pushDeletep(storep);
			}
			oldactivep->sensesStorep(treep);
			oldactivep->sensesp(treep);
		    }
		}
		if (!dlyvscp) {  // First use of this delayed variable
		    string newvarname = (string("__Vdly__")+nodep->varp()->shortName());
		    dlyvscp = createVarSc(oldvscp, newvarname, 0);
		    AstNodeAssign* prep
			= new AstAssignPre (nodep->fileline(),
					    new AstVarRef(nodep->fileline(), dlyvscp, true),
					    new AstVarRef(nodep->fileline(), oldvscp, false));
		    AstNodeAssign* postp
			= new AstAssignPost (nodep->fileline(),
					     new AstVarRef(nodep->fileline(), oldvscp, true),
					     new AstVarRef(nodep->fileline(), dlyvscp, false));
		    postp->lhsp()->user2(true);	// Don't detect this assignment
		    oldvscp->userp(dlyvscp);  // So we can find it later
		    // Make new ACTIVE with identical sensitivity tree
		    AstActive* newactp = new AstActive (nodep->fileline(), "sequentdly",
							m_activep->sensesp());
		    newactp->addStmtsp(prep);	// Add to FRONT of statements
		    newactp->addStmtsp(postp);
		    m_activep->addNext(newactp);					
		    dlyvscp->user2p(newactp);
		}
		AstVarRef* newrefp = new AstVarRef(nodep->fileline(), dlyvscp, true);
		newrefp->user2(true);  // No reason to do it again
		nodep->replaceWith(newrefp); nodep->deleteTree(); nodep=NULL;
	    }
	    else if (!m_inDly && nodep->lvalue()) {
		//UINFO(9,"NBA "<<nodep<<endl);
		if (!m_inInitial) {
		    markVarUsage(nodep->varp(), VU_NONDLY);
		}
	    }
	}
    }

    virtual void visit(AstNodeFor* nodep, AstNUser*) {
	nodep->v3fatalSrc("For statements should have been converted to while statements in V3Unroll\n");
    }
    virtual void visit(AstWhile* nodep, AstNUser*) {
	bool oldloop = m_inLoop;
	m_inLoop = true;
	nodep->iterateChildren(*this);
	m_inLoop = oldloop;
    }

    //--------------------
    // Default: Just iterate
    virtual void visit(AstNode* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
    }

public:
    // CONSTUCTORS
    DelayedVisitor(AstNode* nodep) {
	m_inDly = false;
	m_activep=NULL;
	m_cfuncp=NULL;
	m_nextDlyp=NULL;
	m_inLoop = false;
	m_inInitial = false;

	nodep->accept(*this);
    }
    virtual ~DelayedVisitor() {
	V3Stats::addStat("Optimizations, Delayed shared-sets", m_statSharedSet);
    }
};

//######################################################################
// Delayed class functions

void V3Delayed::delayedAll(AstNetlist* nodep) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    DelayedVisitor visitor (nodep);
}
