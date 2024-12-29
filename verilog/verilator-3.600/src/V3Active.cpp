// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Break always into sensitivity active domains
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
// V3Active's Transformations:
//		
// Note this can be called multiple times.
//   Create a IACTIVE(initial), SACTIVE(combo)
//	ALWAYS: Remove any-edges from sense list
//	    If no POS/NEG in senselist, Fold into SACTIVE(combo)
//	    Else fold into SACTIVE(sequent).
//	    OPTIMIZE: When support async clocks, fold into that active if possible
//	INITIAL: Move into IACTIVE
//	WIRE: Move into SACTIVE(combo)
//
//*************************************************************************

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <map>
#include <algorithm>
#include <vector>

#include "V3Global.h"
#include "V3Active.h"
#include "V3Ast.h"
#include "V3EmitCBase.h"

//***** See below for main transformation engine

//######################################################################
// Collect existing active names

class ActiveBaseVisitor : public AstNVisitor {
protected:
    //int debug() { return 9; }
};

class ActiveNamer : public ActiveBaseVisitor {
private:
    typedef std::map<string,AstActive*> ActiveNameMap;
    // STATE
    AstScope*	m_scopep;		// Current scope to add statement to
    AstActive*	m_iActivep;		// For current scope, the IActive we're building
    AstActive*	m_cActivep;		// For current scope, the SActive(combo) we're building
    vector<AstActive*>	m_activeVec;	// List of sensitive actives, for folding
    // METHODS
    void addActive(AstActive* nodep) {
	if (!m_scopep) nodep->v3fatalSrc("NULL scope");
	m_scopep->addActivep(nodep);
    }
    // VISITORS
    virtual void visit(AstScope* nodep, AstNUser*) {
	m_scopep = nodep;
	m_iActivep = NULL;
	m_cActivep = NULL;
	m_activeVec.clear();
	nodep->iterateChildren(*this);
	// Don't clear scopep, the namer persists beyond this visit
    }
    virtual void visit(AstSenTree* nodep, AstNUser*) {
	// Sort sensitivity list
	nodep->sortSenses();
    }
    // Empty visitors, speed things up
    virtual void visit(AstNodeStmt* nodep, AstNUser*) { }
    //--------------------
    // Default
    virtual void visit(AstNode* nodep, AstNUser*) {
	// Default: Just iterate
	nodep->iterateChildren(*this);
    }
    // METHODS
public:
    AstScope* scopep() { return m_scopep; }
    AstActive* getCActive(FileLine* fl) {
	if (!m_cActivep) {
	    m_cActivep = new AstActive(fl, "combo",
				       new AstSenTree(fl, new AstSenItem(fl,AstSenItem::Combo())));
	    m_cActivep->sensesStorep(m_cActivep->sensesp());
	    addActive(m_cActivep);
	}
	return m_cActivep;
    }
    AstActive* getIActive(FileLine* fl) {
	if (!m_iActivep) {
	    m_iActivep = new AstActive(fl, "initial",
				       new AstSenTree(fl, new AstSenItem(fl,AstSenItem::Initial())));
	    m_iActivep->sensesStorep(m_iActivep->sensesp());
	    addActive(m_iActivep);
	}
	return m_iActivep;
    }
    AstActive* getActive(FileLine* fl, AstSenTree* sensesp) {
	// Return a sentree in this scope that matches given sense list.
	// Not the fastest, but scopes tend to have few clocks
	AstActive* activep = NULL;
	//sitemsp->dumpTree(cout,"  Lookingfor: ");
	for (vector<AstActive*>::iterator it = m_activeVec.begin(); it!=m_activeVec.end(); ++it) {
	    activep = *it;
	    if (activep) {  // Not deleted
		// Compare the list
		AstSenTree* asenp = activep->sensesp();
		if (asenp->sameTree(sensesp)) {
		    UINFO(8,"    Found ACTIVE "<<activep<<endl);
		    goto found;
		}
	    }
	    activep = NULL;
	}
      found:
	// Not found, form a new one
	if (!activep) {
	    AstSenTree* newsenp = sensesp->cloneTree(false)->castSenTree();
	    activep = new AstActive(fl, "sequent", newsenp);
	    activep->sensesStorep(activep->sensesp());
	    UINFO(8,"    New ACTIVE "<<activep<<endl);
	    // Form the sensitivity list
	    addActive(activep);
	    m_activeVec.push_back(activep);
	    // Note actives may have also been added above in the Active visitor
	}
	return activep;
    }
public:
    // CONSTUCTORS
    ActiveNamer() {}
    virtual ~ActiveNamer() {}
    void main(AstScope* nodep) {
	nodep->accept(*this);
    }
};

//######################################################################
// Active AssignDly replacement functions

class ActiveDlyVisitor : public ActiveBaseVisitor {
private:
    // VISITORS
    virtual void visit(AstAssignDly* nodep, AstNUser*) {
	// Convert to a non-delayed assignment
	UINFO(5,"    ASSIGNDLY "<<nodep<<endl);
	nodep->v3warn(COMBDLY,"Delayed assignments (<=) in non-clocked (non flop or latch) blocks should be non-delayed assignments (=).");
	AstNode* newp = new AstAssign (nodep->fileline(),
				       nodep->lhsp()->unlinkFrBack(),
				       nodep->rhsp()->unlinkFrBack());
	nodep->replaceWith(newp);
	nodep->deleteTree(); nodep = NULL;
    }
    // Empty visitors, speed things up
    virtual void visit(AstNodeMath* nodep, AstNUser*) {}
    //--------------------
    virtual void visit(AstNode* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
    }
public:
    // CONSTUCTORS
    ActiveDlyVisitor(AstNode* nodep) {
	nodep->accept(*this);
    }
    virtual ~ActiveDlyVisitor() {}
};

//######################################################################
// Active class functions

class ActiveVisitor : public ActiveBaseVisitor {
private:
    // NODE STATE

    // STATE
    ActiveNamer	m_namer;	// Tracking of active names
    AstCFunc*   m_scopeFinalp;	// Final function for this scope

    // VISITORS
    virtual void visit(AstScope* nodep, AstNUser*) {
	// Create required actives and add to scope
	UINFO(4," SCOPE   "<<nodep<<endl);
	// Clear last scope's names, and collect this scope's existing names
	m_namer.main(nodep);
	m_scopeFinalp = NULL;
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstActive* nodep, AstNUser*) {
	// Actives are being formed, so we can ignore any already made
    }
    virtual void visit(AstInitial* nodep, AstNUser*) {
	// Relink to IACTIVE, unless already under it
	UINFO(4,"    INITIAL "<<nodep<<endl);
	AstActive* wantactivep = m_namer.getIActive(nodep->fileline());
	nodep->unlinkFrBack();
	wantactivep->addStmtsp(nodep);
    }
    virtual void visit(AstAssignAlias* nodep, AstNUser*) {
	// Relink to CACTIVE, unless already under it
	UINFO(4,"    ASSIGNW "<<nodep<<endl);
	AstActive* wantactivep = m_namer.getCActive(nodep->fileline());
	nodep->unlinkFrBack();
	wantactivep->addStmtsp(nodep);
    }
    virtual void visit(AstAssignW* nodep, AstNUser*) {
	// Relink to CACTIVE, unless already under it
	UINFO(4,"    ASSIGNW "<<nodep<<endl);
	AstActive* wantactivep = m_namer.getCActive(nodep->fileline());
	nodep->unlinkFrBack();
	wantactivep->addStmtsp(nodep);
    }
    virtual void visit(AstFinal* nodep, AstNUser*) {
	// Relink to CFUNC for the final
	UINFO(4,"    FINAL "<<nodep<<endl);
	if (!nodep->bodysp()) { // Empty, Kill it.
	    nodep->unlinkFrBack()->deleteTree(); nodep=NULL;
	    return;
	}
	if (!m_scopeFinalp) {
	    m_scopeFinalp = new AstCFunc(nodep->fileline(), "_final", m_namer.scopep());
	    m_scopeFinalp->argTypes(EmitCBaseVisitor::symClassVar());
	    m_scopeFinalp->addInitsp(new AstCStmt(nodep->fileline(),"    "+EmitCBaseVisitor::symTopAssign()+"\n"));
	    m_scopeFinalp->dontCombine(true);
	    m_scopeFinalp->formCallTree(true);
	    m_scopeFinalp->slow(true);
	    m_namer.scopep()->addActivep(m_scopeFinalp);
	}
	nodep->unlinkFrBack();
	m_scopeFinalp->addStmtsp(new AstComment(nodep->fileline(), nodep->typeName()));
	m_scopeFinalp->addStmtsp(nodep->bodysp()->unlinkFrBackWithNext());
	nodep->deleteTree(); nodep = NULL;
    }

    virtual void visit(AstAlways* nodep, AstNUser*) {
	// Move always to appropriate ACTIVE based on its sense list
	UINFO(4,"    ALW   "<<nodep<<endl);
	//if (debug()>=9) nodep->dumpTree(cout,"  Alw: ");

	if (!nodep->bodysp()) {
	    // Empty always.  Kill it.
	    nodep->unlinkFrBack()->deleteTree(); nodep=NULL;
	    return;
	}
	if (nodep->sensesp() && nodep->sensesp()->sensesp() && nodep->sensesp()->sensesp()->isNever()) {
	    // Never executing.  Kill it.
	    if (nodep->sensesp()->sensesp()->nextp()) nodep->v3fatalSrc("Never senitem should be alone, else the never should be eliminated.");
	    nodep->unlinkFrBack()->deleteTree(); nodep=NULL;
	    return;
	}

	// Read sensitivitues
	bool combo = false;
	bool sequent = false;
	if (nodep->sensesp()) {
	    for (AstSenItem* nextp, *senp = nodep->sensesp()->sensesp(); senp; senp=nextp) {
		nextp = senp->nextp()->castSenItem();
		if (senp->edgeType() == AstEdgeType::ANYEDGE) {
		    combo = true;
		    // Delete the sensitivity
		    // We'll add it as a generic COMBO SenItem in a moment.
		    senp->unlinkFrBack()->deleteTree(); senp=NULL;
		} else if (senp->varrefp()) {
		    if (senp->varrefp()->width()>1) senp->v3error("Unsupported: Non-single bit wide signal pos/negedge sensitivity: "
								  <<senp->varrefp()->prettyName());
		    sequent = true;
		    senp->varrefp()->varp()->usedClock(true);
		}
	    }
	}
	if (!combo && !sequent) combo=true;	// If no list, Verilog 2000: always @ (*)
#ifndef NEW_ORDERING
	if (combo && sequent) {
	    nodep->v3error("Unsupported: Mixed edge (pos/negedge) and activity (no edge) sensitive activity list");
	    sequent = false;
	}
#endif

	AstActive* wantactivep = NULL;
	if (combo && !sequent) {
	    // Combo:  Relink to ACTIVE(combo)
	    wantactivep = m_namer.getCActive(nodep->fileline());
	} else {
	    // Sequential: Build a ACTIVE(name)
	    // OPTIMIZE: We could substitute a constant for things in the sense list, for example
	    // always (posedge RESET) { if (RESET).... }  we know RESET is true.
	    // Summarize a long list of combo inputs as just "combo"
	    if (combo) nodep->sensesp()->addSensesp
			   (new AstSenItem(nodep->fileline(),AstSenItem::Combo()));
	    wantactivep = m_namer.getActive(nodep->fileline(), nodep->sensesp());
	}

	// Delete sensitivity list
	if (AstNode* oldsense = nodep->sensesp()) {
	    oldsense->unlinkFrBackWithNext()->deleteTree(); oldsense=NULL;
	}

	// Move node to new active
	nodep->unlinkFrBack();
	wantactivep->addStmtsp(nodep);

	// Warn and/or convert any delayed assignments
	if (combo && !sequent) {
	    ActiveDlyVisitor dlyvisitor (nodep);
	}
    }

    // Empty visitors, speed things up
    virtual void visit(AstNodeMath* nodep, AstNUser*) {}
    virtual void visit(AstVarScope* nodep, AstNUser*) {}
    //--------------------
    virtual void visit(AstNode* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
    }
public:
    // CONSTUCTORS
    ActiveVisitor(AstNode* nodep) {
	m_scopeFinalp = NULL;
	nodep->accept(*this);
    }
    virtual ~ActiveVisitor() {}
};

//######################################################################
// Active class functions

void V3Active::activeAll(AstNetlist* nodep) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    ActiveVisitor visitor (nodep);
}
