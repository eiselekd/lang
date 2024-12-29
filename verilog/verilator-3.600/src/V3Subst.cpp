// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Substitute constants and expressions in expr temp's
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
// V3Subst's Transformations:
//		
// Each module:
//	Search all ASSIGN(WORDSEL(...)) and build what it's assigned to
//	Later usages of that word may then be replaced
//
//*************************************************************************

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <algorithm>
#include <vector>

#include "V3Global.h"
#include "V3Subst.h"
#include "V3Const.h"
#include "V3Stats.h"
#include "V3Ast.h"

//######################################################################
// Common debugging baseclass

class SubstBaseVisitor : public AstNVisitor {
public:
    static int debug() { return 0; }
//    static int debug() { return 9; }
};

//######################################################################
// Subst state, as a visitor of each AstNode

class SubstVarEntry {
    AstVar*		m_varp;		// Variable this tracks
    bool		m_complexAssign;// True if assigned multiple times or in complex mannor
    AstNodeAssign*	m_wholeAssignp;	// Last assignment to entire array
    bool		m_wholeUse;	// True if used as entire wide array
    bool		m_wordAssign;	// True if any word assignments
    bool		m_wordUse;	// True if any individual word usage
    vector<AstNodeAssign*>	m_wordAssignps;	// Last assignment to each word of this var
    vector<bool>	m_wordUses;	// True if each word was consumed
    vector<bool>	m_wordComplex;	// True if each word is complex
    int debug() { return SubstBaseVisitor::debug(); }
public:
    SubstVarEntry (AstVar* varp) {	// Construction for when a var is used
	m_varp = varp;
	m_complexAssign = false;
	m_wholeAssignp = NULL;
	m_wholeUse = false;
	m_wordAssign = false;
	m_wordUse = false;
	m_wordAssignps.resize(varp->widthWords());
	m_wordUses.resize(varp->widthWords());
	m_wordComplex.resize(varp->widthWords());
	for (int i=0; i<varp->widthWords(); i++) {
	    m_wordAssignps[i] = NULL;
	    m_wordUses[i] = false;
	    m_wordComplex[i] = false;
	}
    }
    bool wordNumOk(int word) {
	return word < m_varp->widthWords();
    }
    AstNodeAssign* getWordAssignp(int word) {
	if (!wordNumOk(word)) return NULL;
	else return m_wordAssignps[word];
    }
    void assignWhole (AstNodeAssign* assp) {
	if (m_wholeAssignp) m_complexAssign = true;
	m_wholeAssignp = assp;
    }
    void assignWord (int word, AstNodeAssign* assp) {
	if (!wordNumOk(word) || getWordAssignp(word) || m_wordComplex[word]) m_complexAssign = true;
	m_wordAssign = true;
	if (wordNumOk(word)) m_wordAssignps[word] = assp;
    }
    void assignWordComplex (int word) {
	if (!wordNumOk(word) || getWordAssignp(word) || m_wordComplex[word]) m_complexAssign = true;
	m_wordComplex[word] = true;
    }
    void assignComplex() {
	m_complexAssign = true;
    }
    void consumeWhole() {  //==consumeComplex as we don't know the difference
	m_wholeUse = true;
    }
    void consumeWord(int word) {
	m_wordUses[word] = true;
	m_wordUse = true;
    }
    // ACCESSORS
    AstNode* substWhole(AstNode* errp) {
	if (!m_varp->isWide()
	    && !m_complexAssign && m_wholeAssignp && !m_wordAssign) {
	    AstNodeAssign* assp = m_wholeAssignp;
	    if (!assp) errp->v3fatalSrc("Reading whole that was never assigned");
	    return (assp->rhsp());
	} else {
	    return NULL;
	}
    }
    AstNode* substWord(AstNode* errp, int word) {  // Return what to substitute given word number for
	if (!m_complexAssign && !m_wholeAssignp && !m_wordComplex[word]) {
	    AstNodeAssign* assp = getWordAssignp(word);
	    if (!assp) errp->v3fatalSrc("Reading a word that was never assigned, or bad word #");
	    return (assp->rhsp());
	} else {
	    return NULL;
	}
    }
    void deleteAssign (AstNodeAssign* nodep) {
	UINFO(5, "Delete "<<nodep<<endl);
	nodep->unlinkFrBack()->deleteTree(); nodep=NULL;
    }
    void deleteUnusedAssign() {
	// If there are unused assignments in this var, kill them
	if (!m_wholeUse && !m_wordUse && m_wholeAssignp) {
	    deleteAssign (m_wholeAssignp); m_wholeAssignp=NULL;
	}
	for (unsigned i=0; i<m_wordUses.size(); i++) {
	    if (!m_wholeUse && !m_wordUses[i] && m_wordAssignps[i] && !m_wordComplex[i]) {
		deleteAssign (m_wordAssignps[i]); m_wordAssignps[i]=NULL;
	    }
	}
    }
};

//######################################################################

class SubstVisitor : public SubstBaseVisitor {
private:
    // NODE STATE
    // Passed to SubstElimVisitor
    // AstVar::userp		-> SubstVar* for usage var, 0=not set yet
    //
    // STATE
    vector<SubstVarEntry*>	m_entryps;	// Nodes to delete when we are finished
    int				m_ops;		// Number of operators on assign rhs
    V3Double0			m_statSubsts;	// Statistic tracking

    enum { SUBST_MAX_OPS_SUBST = 30,		// Maximum number of ops to substitute in
	   SUBST_MAX_OPS_NA = 9999 };		// Not allowed to substitute

    // METHODS
    SubstVarEntry* getEntryp(AstVarRef* nodep) {
	if (!nodep->varp()->userp()) {
	    SubstVarEntry* entryp = new SubstVarEntry (nodep->varp());
	    m_entryps.push_back(entryp);
	    nodep->varp()->userp(entryp);
	    return entryp;
	} else {
	    SubstVarEntry* entryp = (SubstVarEntry*)(nodep->varp()->userp());
	    return entryp;
	}
    }

    // VISITORS
    virtual void visit(AstNodeAssign* nodep, AstNUser*) {
	m_ops = 0;
	nodep->rhsp()->iterateAndNext(*this);
	bool hit=false;
	if (AstVarRef* varrefp = nodep->lhsp()->castVarRef()) {
	    if (varrefp->varp()->isStatementTemp()) {
		SubstVarEntry* entryp = getEntryp(varrefp);
		hit = true;
		if (m_ops > SUBST_MAX_OPS_SUBST) {
		    UINFO(8," ASSIGNtooDeep "<<varrefp<<endl);
		    entryp->assignComplex();
		} else {
		    UINFO(8," ASSIGNwhole "<<varrefp<<endl);
		    entryp->assignWhole(nodep);
		}
	    }
	}
	else if (AstWordSel* wordp = nodep->lhsp()->castWordSel()) {
	    if (AstVarRef* varrefp = wordp->lhsp()->castVarRef()) {
		if (wordp->rhsp()->castConst()
		    && varrefp->varp()->isStatementTemp()) {
		    int word = wordp->rhsp()->castConst()->asInt();
		    SubstVarEntry* entryp = getEntryp(varrefp);
		    hit = true;
		    if (m_ops > SUBST_MAX_OPS_SUBST) {
			UINFO(8," ASSIGNtooDeep "<<varrefp<<endl);
			entryp->assignWordComplex(word);
		    } else {
			UINFO(8," ASSIGNword"<<word<<" "<<varrefp<<endl);
			entryp->assignWord(word, nodep);
		    }
		}
	    }
	}
	if (!hit) {
	    nodep->lhsp()->accept(*this);
	}
    }
    void replaceSubstEtc(AstNode* nodep, AstNode* substp) {
	if (debug()>5) nodep->dumpTree(cout,"  substw_old: ");
	AstNode* newp = substp->cloneTree(true);
	if (!nodep->isQuad() && newp->isQuad()) {
	    newp = new AstCast (newp->fileline(), newp, nodep);
	}
	if (debug()>5)  newp->dumpTree(cout,"       w_new: ");
	nodep->replaceWith(newp);
	pushDeletep(nodep);  nodep=NULL;
	m_statSubsts++;
    }
    virtual void visit(AstWordSel* nodep, AstNUser*) {
	nodep->rhsp()->accept(*this);
	AstVarRef* varrefp = nodep->lhsp()->castVarRef();
	AstConst* constp = nodep->rhsp()->castConst();
	if (varrefp && varrefp->varp()->isStatementTemp()
	    && !varrefp->lvalue()
	    && constp) {
	    // Nicely formed lvalues handled in NodeAssign
	    // Other lvalues handled as unknown mess in AstVarRef
	    int word = constp->asInt();
	    UINFO(8," USEword"<<word<<" "<<varrefp<<endl);
	    SubstVarEntry* entryp = getEntryp(varrefp);
	    if (AstNode* substp = entryp->substWord (nodep, word)) {
		replaceSubstEtc(nodep, substp); nodep=NULL;
	    } else {
		entryp->consumeWord(word);
	    }
	} else {
	    nodep->lhsp()->accept(*this);
	}
    }
    virtual void visit(AstVarRef* nodep, AstNUser*) {
	if (nodep->varp()->isStatementTemp()) {
	    SubstVarEntry* entryp = getEntryp (nodep);
	    if (nodep->lvalue()) {
		UINFO(8," ASSIGNcpx "<<nodep<<endl);
		entryp->assignComplex();
	    } else if (AstNode* substp = entryp->substWhole(nodep)) {
		UINFO(8," USEwhole "<<nodep<<endl);
		replaceSubstEtc(nodep, substp); nodep=NULL;
	    } else {  // Consumed w/o substitute
		UINFO(8," USEwtf   "<<nodep<<endl);
		entryp->consumeWhole();
	    }
	}
    }
    virtual void visit(AstVar* nodep, AstNUser*) {}
    virtual void visit(AstNode* nodep, AstNUser*) {
	m_ops++;
	if (!nodep->isSubstOptimizable()) {
	    m_ops = SUBST_MAX_OPS_NA;
	}
	nodep->iterateChildren(*this);
    }
public:
    // CONSTUCTORS
    SubstVisitor(AstNode* nodep) {
	AstNode::userClearTree();	// userp() used on entire tree
	m_ops = 0;
	nodep->accept(*this);
    }
    virtual ~SubstVisitor() {
	V3Stats::addStat("Optimizations, Substituted temps", m_statSubsts);
	for (vector<SubstVarEntry*>::iterator it = m_entryps.begin(); it != m_entryps.end(); ++it) {
	    (*it)->deleteUnusedAssign();
	    delete (*it);
	}
    }
};

//######################################################################
// Subst class functions

void V3Subst::substituteAll(AstNetlist* nodep) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    SubstVisitor visitor (nodep);
}
