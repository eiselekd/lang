// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Waves tracing
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
// V3Trace's Transformations:
//  Pass 1:
//	For each TRACE
//		Add to list of traces, Mark where traces came from, unlink it
//		Look for duplicate values; if so, cross reference
//		Make vertex for each var it references
//  Pass 2:
//	Go through _eval; if there's a call on the same flat statement list
//	then all functions for that call can get the same activity code.
//	Likewise, all _slow functions can get the same code.
//	CFUNCs that are public need unique codes, as does _eval
//
//	For each CFUNC with unique callReason
//		Make vertex
//		For each var it sets, make vertex and edge from cfunc vertex
//
//	For each CFUNC in graph
//		Add ASSIGN(SEL(__Vm_funcActivity,activityNumber++),1)
//	Create 	__Vm_funcActivity vector
//	Sort TRACEs by activityNumber(s) they come from (may be more then one)
//	Each set of activityNumbers
//		Add IF (SEL(__Vm_funcActivity,activityNumber),1)
//		Add traces under that activity number.
//	Assign trace codes:
//		If from a VARSCOPE, record the trace->varscope map
//		Else, assign trace codes to each variable
//		
//*************************************************************************

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <set>
#include <map>

#include "V3Global.h"
#include "V3Trace.h"
#include "V3EmitCBase.h"
#include "V3Graph.h"
#include "V3Hashed.h"
#include "V3Stats.h"

//######################################################################
// Graph vertexes

class TraceActivityVertex : public V3GraphVertex {
    AstNode*	m_insertp;	// Insert before this statement
    vlsint32_t	m_activityCode;
    bool	m_activityCodeValid;
    bool	m_slow;		// If always slow, we can use the same code
public:
    TraceActivityVertex(V3Graph* graphp, AstNode* nodep, bool slow)
	: V3GraphVertex(graphp), m_insertp(nodep) {
	m_activityCode = 0;
	m_activityCodeValid = false;
	m_slow = slow;
    }
    enum { ACTIVITY_NEVER=(1UL<<31) };
    enum { ACTIVITY_ALWAYS=-1 };
    enum { ACTIVITY_SLOW=0 };
    class ActivityAlways {};
    TraceActivityVertex(V3Graph* graphp, vlsint32_t code)
	: V3GraphVertex(graphp) {
	activityCode(code);
	m_slow = false;
    }
    virtual ~TraceActivityVertex() {}
    // Accessors
    AstNode* insertp() const {
	if (!m_insertp) v3fatal("Null insertp; probably called on a special always/slow.");
	return m_insertp;
    }
    virtual string name() const {
	if (activityAlways()) return "*ALWAYS*";
	else return ((string)(slow()?"*SLOW* ":""))+insertp()->name();
    }
    virtual string dotColor() const { return slow()?"yellowGreen":"green"; }
    bool activityCodeValid() const { return m_activityCodeValid; }
    vlsint32_t activityCode() const { return m_activityCode; }
    bool activityAlways() const { return activityCode()==ACTIVITY_ALWAYS; }
    void activityCode(vlsint32_t code) { m_activityCode=code; m_activityCodeValid=true;}
    bool slow() const { return m_slow; }
    void slow(bool flag) { if (!flag) m_slow=false; }
};

class TraceCFuncVertex : public V3GraphVertex {
    AstCFunc*	m_nodep;
public:
    TraceCFuncVertex(V3Graph* graphp, AstCFunc* nodep)
	: V3GraphVertex(graphp), m_nodep(nodep) {
    }
    virtual ~TraceCFuncVertex() {}
    // Accessors
    AstCFunc* nodep() const { return m_nodep; }
    virtual string name() const { return nodep()->name(); }
    virtual string dotColor() const { return "yellow"; }
};

class TraceTraceVertex : public V3GraphVertex {
    AstTraceInc*	m_nodep;		// TRACEINC this represents
    TraceTraceVertex*	m_duplicatep;		// NULL, or other vertex with the real code() that duplicates this one
public:
    TraceTraceVertex(V3Graph* graphp, AstTraceInc* nodep)
	: V3GraphVertex(graphp), m_nodep(nodep), m_duplicatep(NULL) {}
    virtual ~TraceTraceVertex() {}
    // Accessors
    AstTraceInc* nodep() const { return m_nodep; }
    virtual string name() const { return nodep()->declp()->name(); }
    virtual string dotColor() const { return "red"; }
    TraceTraceVertex* duplicatep() const { return m_duplicatep; }
    void duplicatep(TraceTraceVertex* dupp) {
	if (duplicatep()) nodep()->v3fatalSrc("Assigning duplicatep() to already duplicated node");
	m_duplicatep=dupp; }
};

class TraceVarVertex : public V3GraphVertex {
    AstVarScope*	m_nodep;
public:
    TraceVarVertex(V3Graph* graphp, AstVarScope* nodep)
	: V3GraphVertex(graphp), m_nodep(nodep) {}
    virtual ~TraceVarVertex() {}
    // Accessors
    AstVarScope* nodep() const { return m_nodep; }
    virtual string name() const { return nodep()->name(); }
    virtual string dotColor() const { return "skyblue"; }
};

//######################################################################
// Trace state, as a visitor of each AstNode

class TraceVisitor : public EmitCBaseVisitor {
private:
    // NODE STATE
    // V3Hashed
    //  Ast*::user4()			// V3Hashed calculation
    // Cleared entire netlist	
    //  AstCFunc::user()		// V3GraphVertex* for this node
    //  AstTraceInc::user()		// V3GraphVertex* for this node
    //  AstVarScope::user()		// V3GraphVertex* for this node
    //  AstCCall::user2()		// bool; walked next list for other ccalls
    //  Ast*::user3()			// TraceActivityVertex* for this node

    // STATE
    AstModule*		m_topModp;	// Module to add variables to
    AstScope*		m_highScopep;	// Scope to add variables to
    AstCFunc*		m_funcp;	// C function adding to graph
    AstTraceInc*	m_tracep;	// Trace function adding to graph
    AstCFunc*		m_initFuncp;	// Trace function we add statements to
    AstCFunc*		m_fullFuncp;	// Trace function we add statements to
    AstCFunc*		m_chgFuncp;	// Trace function we add statements to
    AstVarScope*	m_activityVscp;	// Activity variable
    uint32_t		m_code;		// Trace ident code# being assigned
    V3Graph		m_graph;	// Var/CFunc tracking
    TraceActivityVertex* m_alwaysVtxp;	// "Always trace" vertex
    bool		m_finding;	// Pass one of algorithm?

    V3Double0		m_statChgSigs;	// Statistic tracking
    V3Double0		m_statUniqSigs;	// Statistic tracking
    V3Double0		m_statUniqCodes;// Statistic tracking
    //int debug() { return 9; }

    // METHODS
    void detectDuplicates() {
	UINFO(9,"Finding duplicates\n");
	// Note uses user4
	V3Hashed  hashed;	// Duplicate code detection
	// Hash all of the values the traceIncs need
	for (V3GraphVertex* itp = m_graph.verticesBeginp(); itp; itp=itp->verticesNextp()) {
	    if (TraceTraceVertex* vvertexp = dynamic_cast<TraceTraceVertex*>(itp)) {
		AstTraceInc* nodep = vvertexp->nodep();
		if (nodep->valuep()) {
		    if (nodep->valuep()->backp() != nodep) nodep->v3fatalSrc("Trace duplicate back needs consistency, so we can map duplicates back to TRACEINCs");
		    hashed.hashAndInsert(nodep->valuep());
		    UINFO(8, "  Hashed "<<hex<<nodep->valuep()->user4()<<" "<<nodep<<endl);
		}
	    }
	}
	// Find if there are any duplicates
	for (V3GraphVertex* itp = m_graph.verticesBeginp(); itp; itp=itp->verticesNextp()) {
	    if (TraceTraceVertex* vvertexp = dynamic_cast<TraceTraceVertex*>(itp)) {
		AstTraceInc* nodep = vvertexp->nodep();
		if (nodep->valuep() && !vvertexp->duplicatep()) {
		    V3Hashed::iterator dupit = hashed.findDuplicate(nodep->valuep());
		    if (dupit != hashed.end()) {
			AstTraceInc* dupincp = hashed.iteratorNodep(dupit)->backp()->castTraceInc();
			if (!dupincp) nodep->v3fatalSrc("Trace duplicate of wrong type");
			TraceTraceVertex* dupvertexp = dynamic_cast<TraceTraceVertex*>(dupincp->userp()->castGraphVertex());
			UINFO(8,"  Orig "<<nodep<<endl);
			UINFO(8,"   dup "<<dupincp<<endl);
			// Mark the found node as a duplicate of the first node
			// (Not vice-versa as we get the iterator for the found node)
			dupvertexp->duplicatep(vvertexp);
			// Remove node from comparison so don't hit it again
			hashed.erase(dupit);
		    }
		}
	    }
	}
	AstNode::user4ClearTree();
    }

    void graphSimplify() {
	// Remove all variable nodes
	for (V3GraphVertex* nextp, *itp = m_graph.verticesBeginp(); itp; itp=nextp) {
	    nextp = itp->verticesNextp();
	    if (TraceVarVertex* vvertexp = dynamic_cast<TraceVarVertex*>(itp)) {
		vvertexp->rerouteEdges(&m_graph);
		vvertexp->unlinkDelete(&m_graph);
	    }
	}
	// Remove multiple variables connecting funcs to traces
	// We do this twice, as then we have fewer edges to multiply out in the below expansion.
	m_graph.removeRedundantEdges(&V3GraphEdge::followAlwaysTrue);
	// Remove all Cfunc nodes
	for (V3GraphVertex* nextp, *itp = m_graph.verticesBeginp(); itp; itp=nextp) {
	    nextp = itp->verticesNextp();
	    if (TraceCFuncVertex* vvertexp = dynamic_cast<TraceCFuncVertex*>(itp)) {
		vvertexp->rerouteEdges(&m_graph);
		vvertexp->unlinkDelete(&m_graph);
	    }
	}

	// Remove multiple variables connecting funcs to traces
	m_graph.removeRedundantEdges(&V3GraphEdge::followAlwaysTrue);

	// If there are any edges from a always, keep only the always
	for (V3GraphVertex* itp = m_graph.verticesBeginp(); itp; itp=itp->verticesNextp()) {
	    if (TraceTraceVertex* vvertexp = dynamic_cast<TraceTraceVertex*>(itp)) {
		V3GraphEdge* alwaysEdgep = NULL;
		for (V3GraphEdge* edgep = vvertexp->inBeginp(); edgep; edgep=edgep->inNextp()) {
		    TraceActivityVertex* actVtxp = dynamic_cast<TraceActivityVertex*>(edgep->fromp());
		    if (!actVtxp) vvertexp->nodep()->v3fatalSrc("Tracing a node with FROM non activity");
		    if (actVtxp->activityAlways()) {
			alwaysEdgep = edgep;
			break;
		    }
		}
		if (alwaysEdgep) {
		    for (V3GraphEdge* nextp, *edgep = vvertexp->inBeginp(); edgep; edgep=nextp) {
			nextp = edgep->inNextp();
			if (edgep!=alwaysEdgep) edgep->unlinkDelete();
		    }
		}
	    }
	}

	// Activity points with no outputs can be removed
	for (V3GraphVertex* nextp, *itp = m_graph.verticesBeginp(); itp; itp=nextp) {
	    nextp = itp->verticesNextp();
	    if (TraceActivityVertex* vvertexp = dynamic_cast<TraceActivityVertex*>(itp)) {
		if (!vvertexp->outBeginp()) {
		    vvertexp->unlinkDelete(&m_graph);
		}
	    }
	}
    }

    void assignActivity() {
	// Select activity numbers and put into each CFunc vertex
	uint32_t activityNumber = 1;	// Note 0 indicates "slow"
	for (V3GraphVertex* itp = m_graph.verticesBeginp(); itp; itp=itp->verticesNextp()) {
	    if (TraceActivityVertex* vvertexp = dynamic_cast<TraceActivityVertex*>(itp)) {
		if (!vvertexp->activityCodeValid()) {
		    if (vvertexp->slow()) {
			// If all functions in the calls are slow, we'll share the same code.
			// This makes us need less activityNumbers and so speeds up the fast path.
			vvertexp->activityCode(TraceActivityVertex::ACTIVITY_SLOW);
		    } else {
			vvertexp->activityCode(activityNumber++);
		    }
		}
	    }
	}

	// Insert global variable
	if (!activityNumber) activityNumber++;   // For simplicity, always create it
	activityNumber = VL_WORDS_I(activityNumber)*VL_WORDSIZE;   // For tighter code; round to next 32 bit point.
	AstVar* newvarp = new AstVar (m_chgFuncp->fileline(), AstVarType::MODULETEMP,
				      "__Vm_traceActivity",
				      new AstRange (m_chgFuncp->fileline(), activityNumber-1, 0));
	m_topModp->addStmtp(newvarp);
	AstVarScope* newvscp = new AstVarScope(newvarp->fileline(), m_highScopep, newvarp);
	m_highScopep->addVarp(newvscp);
	m_activityVscp = newvscp;

	// Insert activity setter
	for (V3GraphVertex* itp = m_graph.verticesBeginp(); itp; itp=itp->verticesNextp()) {
	    if (TraceActivityVertex* vvertexp = dynamic_cast<TraceActivityVertex*>(itp)) {
		if (!vvertexp->activityAlways()) {
		    FileLine* fl = vvertexp->insertp()->fileline();
		    uint32_t acode = vvertexp->activityCode();
		    vvertexp->insertp()->addNextHere
			(new AstAssign (fl,
					new AstSel (fl, new AstVarRef(fl, m_activityVscp, true),
						    new AstConst(fl, acode),
						    new AstConst(fl, 1)),
					new AstConst (fl, V3Number(fl, 1, 1))));
		}
	    }
	}
    }

    void putTracesIntoTree() {
	// Form a sorted list of the traces we are interested in
	UINFO(9,"Making trees\n");

	typedef set<uint32_t> ActCodeSet;	// All activity numbers applying to a given trace
	typedef multimap<ActCodeSet,TraceTraceVertex*> TraceVec;	// For activity set, what traces apply
	TraceVec traces;

	// Form sort structure
	// If a trace doesn't have activity, it's constant, and we don't need to track changes on it.
	for (V3GraphVertex* itp = m_graph.verticesBeginp(); itp; itp=itp->verticesNextp()) {
	    if (TraceTraceVertex* vvertexp = dynamic_cast<TraceTraceVertex*>(itp)) {
		ActCodeSet actset;
		UINFO(9,"  Add to sort: "<<vvertexp<<endl);
		if (debug()>=9) vvertexp->nodep()->dumpTree(cout,"-   trnode: ");
		for (V3GraphEdge* edgep = vvertexp->inBeginp(); edgep; edgep=edgep->inNextp()) {
		    TraceActivityVertex* cfvertexp = dynamic_cast<TraceActivityVertex*>(edgep->fromp());
		    if (!cfvertexp) vvertexp->nodep()->v3fatalSrc("Should have been function pointing to this trace");
		    UINFO(9,"   Activity: "<<cfvertexp<<endl);
		    if (cfvertexp->activityAlways()) {
			// If code 0, we always trace; ignore other codes
			actset.clear();
			actset.insert(TraceActivityVertex::ACTIVITY_ALWAYS);
			break;
		    } else {
			uint32_t acode = cfvertexp->activityCode();
			if (actset.find(acode) == actset.end()) {
			    actset.insert(acode);
			}
		    }
		}
		// If a trace doesn't have activity, it's constant, and we don't need to track changes on it.
		// We put constants and non-changers last, as then the prevvalue vector is more compacted
		if (actset.empty()) actset.insert(TraceActivityVertex::ACTIVITY_NEVER);
		traces.insert(make_pair(actset, vvertexp));
	    }
	}

	// Our keys are now sorted to have same activity number adjacent,
	// then by trace order.  (Better would be execution order for cache efficiency....)
	// Last are constants and non-changers, as then the last value vector is more compact

	// Put TRACEs back into the tree
	const ActCodeSet* lastactp = NULL;
	AstNode* lastnodep = NULL;
	for (TraceVec::iterator it = traces.begin(); it!=traces.end(); ++it) {
	    const ActCodeSet& actset = it->first;
	    TraceTraceVertex* vvertexp = it->second;
	    UINFO(9,"  Done sort: "<<vvertexp<<endl);
	    bool needChg = true;
	    if (actset.find(TraceActivityVertex::ACTIVITY_NEVER) != actset.end()) {
		// No activity needed; it's a constant value or only set in initial block
		needChg = false;
	    }
	    AstNode* addp = assignTraceCode(vvertexp, vvertexp->nodep(), needChg);
	    if (addp) {	 // Else no activity or duplicate
		if (actset.find(TraceActivityVertex::ACTIVITY_NEVER) != actset.end()) {
		    vvertexp->nodep()->v3fatalSrc("If never, needChg=0 and shouldn't need to add.");
		} else if (actset.find(TraceActivityVertex::ACTIVITY_ALWAYS) != actset.end()) {
		    // Must always set it; add to base of function
		    m_chgFuncp->addStmtsp(addp);
		} else if (lastactp && actset == *lastactp && lastnodep) {
		    // Add to last statement we built
		    lastnodep->addNext(addp);
		    lastnodep = addp;
		} else {
		    // Build a new IF statement
		    FileLine* fl = addp->fileline();
		    AstNode* condp = NULL;
		    for (ActCodeSet::iterator csit = actset.begin(); csit!=actset.end(); ++csit) {
			uint32_t acode = *csit;
			AstNode* selp = new AstSel (fl, new AstVarRef(fl, m_activityVscp, false),
						    new AstConst(fl, acode),
						    new AstConst(fl, 1));
			if (condp) condp = new AstOr (fl, condp, selp);
			else condp = selp;
		    }
		    AstIf* ifp = new AstIf (fl, condp, addp, NULL);
		    ifp->branchPred(AstBranchPred::UNLIKELY);
		    m_chgFuncp->addStmtsp(ifp);
		    lastactp = &actset;
		    lastnodep = addp;
		}
	    }
	}

	// Set in initializer

	// Clear activity after tracing completes
	FileLine* fl = m_chgFuncp->fileline();
	AstNode* clrp = new AstAssign (fl,
				       new AstVarRef(fl, m_activityVscp, true),
				       new AstConst(fl, V3Number(fl, m_activityVscp->width())));
	m_fullFuncp->addFinalsp(clrp->cloneTree(true));
	m_chgFuncp->addFinalsp(clrp);
    }

    uint32_t assignDeclCode(AstTraceDecl* nodep) {
	if (!nodep->code()) {
	    nodep->code(m_code);
	    m_code += nodep->codeInc();
	    m_statUniqCodes += nodep->codeInc();
	    m_statUniqSigs++;
	}
	return nodep->code();
    }

    AstNode* assignTraceCode(TraceTraceVertex* vvertexp, AstTraceInc* nodep, bool needChg) {
	// Assign trace code, add to tree, return node for change tree or null
	// Look for identical copies
	uint32_t codePreassigned = 0;
	//if (debug()>=9) nodep->dumpTree(cout,"-   assnnode: ");
	// Find non-duplicated node; note some nodep's maybe null, as they were deleted below
	TraceTraceVertex* dupvertexp = vvertexp;
	while (dupvertexp->duplicatep()) {
	    dupvertexp = dupvertexp->duplicatep();
	    UINFO(9,"   dupOf "<<((void*)dupvertexp)<<" "<<((void*)dupvertexp->nodep())
		  <<" "<<dupvertexp<<endl);
	}
	if (dupvertexp != vvertexp) {
	    // It's an exact copy.  We'll assign the code to the master on
	    // the first one we hit; the later ones will share the code.
	    codePreassigned = assignDeclCode(dupvertexp->nodep()->declp());
	    nodep->declp()->code(codePreassigned);
	} else {
	    assignDeclCode(nodep->declp());
	}
	UINFO(8,"   Created code="<<nodep->declp()->code()
	      <<" "<<(codePreassigned?"[PREASS]":"")
	      <<" "<<(needChg?"[CHG]":"")<<" "<<nodep<<endl);

	AstNode* incAddp = NULL;
	if (!codePreassigned) {
	    // Add to trace cfuncs
	    if (needChg) {
		m_statChgSigs++;
		incAddp = nodep->cloneTree(true);
	    }
	    m_fullFuncp->addStmtsp(nodep);
	} else {
	    // Duplicates don't need a TraceInc
	    pushDeletep(nodep); nodep=NULL;
	}
	return incAddp;
    }

    TraceCFuncVertex* getCFuncVertexp(AstCFunc* nodep) {
	TraceCFuncVertex* vertexp = dynamic_cast<TraceCFuncVertex*>(nodep->userp()->castGraphVertex());
	if (!vertexp) {
	    vertexp = new TraceCFuncVertex(&m_graph, nodep);
	    nodep->userp(vertexp);
	}
	return vertexp;
    }
    TraceActivityVertex* getActivityVertexp(AstNode* nodep, bool slow) {
	TraceActivityVertex* vertexp = dynamic_cast<TraceActivityVertex*>(nodep->user3p()->castGraphVertex());
	if (!vertexp) {
	    vertexp = new TraceActivityVertex(&m_graph, nodep, slow);
	    nodep->user3p(vertexp);
	}
	vertexp->slow(slow);
	return vertexp;
    }

    // VISITORS
    virtual void visit(AstNetlist* nodep, AstNUser*) {
	AstNode::userClearTree();
	AstNode::user2ClearTree();
	AstNode::user3ClearTree();
	m_code = 1; 	// Multiple TopScopes will require fixing how code#s
	// are assigned as duplicate varscopes must result in the same tracing code#.

	// Make a always vertex
	m_alwaysVtxp = new TraceActivityVertex(&m_graph, TraceActivityVertex::ACTIVITY_ALWAYS);

	// Add vertexes for all TRACES, and edges from VARs each trace looks at
	m_finding = false;
	nodep->iterateChildren(*this);

	// Add vertexes for all CFUNCs, and edges to VARs the func sets
	m_finding = true;
	nodep->iterateChildren(*this);
	m_finding = false;

	// Detect and remove duplicate values
	detectDuplicates();

	// Simplify it
	if (debug()>=6) m_graph.dumpDotFilePrefixed("trace_pre");
	graphSimplify();
	if (debug()>=6) m_graph.dumpDotFilePrefixed("trace_opt");

	// Create new TRACEINCs
	assignActivity();
	putTracesIntoTree();
    }
    virtual void visit(AstModule* nodep, AstNUser*) {
	if (nodep->isTop()) m_topModp = nodep;
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstTopScope* nodep, AstNUser*) {
	AstScope* scopep = nodep->scopep();
	if (!scopep) nodep->v3fatalSrc("No scope found on top level");
	m_highScopep = scopep;
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstCCall* nodep, AstNUser*) {
	UINFO(8,"   CCALL "<<nodep<<endl);
	if (!m_finding && !nodep->user2()) {
	    // See if there are other calls in same statement list;
	    // If so, all funcs might share the same activity code
	    TraceActivityVertex* activityVtxp = getActivityVertexp(nodep, nodep->funcp()->slow());
	    for (AstNode* nextp=nodep; nextp; nextp=nextp->nextp()) {
		if (AstCCall* ccallp = nextp->castCCall()) {
		    ccallp->user2(true); // Processed
		    UINFO(8,"     SubCCALL "<<ccallp<<endl);
		    V3GraphVertex* ccallFuncVtxp = getCFuncVertexp(ccallp->funcp());
		    activityVtxp->slow(ccallp->funcp()->slow());
		    new V3GraphEdge (&m_graph, activityVtxp, ccallFuncVtxp, 1);
		}
	    }
	}
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstCFunc* nodep, AstNUser*) {
	UINFO(8,"   CFUNC "<<nodep<<endl);
 	if (nodep->funcType() == AstCFuncType::TRACE_INIT) {
	    m_initFuncp = nodep;
	} else if (nodep->funcType() == AstCFuncType::TRACE_FULL) {
	    m_fullFuncp = nodep;
	} else if (nodep->funcType() == AstCFuncType::TRACE_CHANGE) {
	    m_chgFuncp = nodep;
	}
	V3GraphVertex* funcVtxp = new TraceCFuncVertex(&m_graph, nodep);
	if (!m_finding) {  // If public, we need a unique activity code to allow for sets directly in this func
	    if (nodep->funcPublic() || nodep->name() == "_eval") {
		// Need a non-null place to remember to later add a statement; make one
		if (!nodep->stmtsp()) nodep->addStmtsp(new AstComment(nodep->fileline(), "Tracing activity check"));
		V3GraphVertex* activityVtxp = getActivityVertexp(nodep->stmtsp(), nodep->slow());
		new V3GraphEdge (&m_graph, activityVtxp, funcVtxp, 1);
	    }
	}
	m_funcp = nodep;
	nodep->iterateChildren(*this);
	m_funcp = NULL;
    }
    virtual void visit(AstTraceInc* nodep, AstNUser*) {
	UINFO(8,"   TRACE "<<nodep<<endl);
	if (m_finding) nodep->v3fatalSrc("Traces should have been removed in prev step.");
	nodep->unlinkFrBack();

	V3GraphVertex* vertexp = new TraceTraceVertex(&m_graph, nodep);
	nodep->userp(vertexp);

	if (!m_funcp || (!m_chgFuncp || !m_fullFuncp)) nodep->v3fatalSrc("Trace not under func");
	m_tracep = nodep;
	nodep->iterateChildren(*this);
	m_tracep = NULL;
    }
    virtual void visit(AstVarRef* nodep, AstNUser*) {
	if (m_tracep) {
	    if (!nodep->varScopep()) nodep->v3fatalSrc("No var scope?");
	    if (nodep->lvalue()) nodep->v3fatalSrc("Lvalue in trace?  Should be const.");
	    V3GraphVertex* varVtxp = nodep->varScopep()->userp()->castGraphVertex();
	    if (!varVtxp) {
		varVtxp = new TraceVarVertex(&m_graph, nodep->varScopep());
		nodep->varScopep()->userp(varVtxp);
	    }
	    V3GraphVertex* traceVtxp = m_tracep->userp()->castGraphVertex();
	    new V3GraphEdge(&m_graph, varVtxp, traceVtxp, 1);
	    if (nodep->varp()->isPrimaryIn()) {   // Always need to trace primary inputs
		new V3GraphEdge(&m_graph, m_alwaysVtxp, traceVtxp, 1);
	    }
	}
	else if (m_funcp && m_finding && nodep->lvalue()) {
	    if (!nodep->varScopep()) nodep->v3fatalSrc("No var scope?");
	    V3GraphVertex* funcVtxp = getCFuncVertexp(m_funcp);
	    V3GraphVertex* varVtxp = nodep->varScopep()->userp()->castGraphVertex();
	    if (varVtxp) { // else we're not tracing this signal
		new V3GraphEdge(&m_graph, funcVtxp, varVtxp, 1);
	    }
	}
    }
    //--------------------
    virtual void visit(AstNode* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
    }

public:
    // CONSTUCTORS
    TraceVisitor(AstNetlist* nodep) {
	m_funcp = NULL;
	m_tracep = NULL;
	m_topModp = NULL;
	m_highScopep = NULL;
	m_finding = false;
	m_activityVscp = NULL;
	m_alwaysVtxp = NULL;
	m_initFuncp = NULL;
	m_fullFuncp = NULL;
	m_chgFuncp = NULL;
	nodep->accept(*this);
    }
    virtual ~TraceVisitor() {
	V3Stats::addStat("Tracing, Unique changing signals", m_statChgSigs);
	V3Stats::addStat("Tracing, Unique traced signals", m_statUniqSigs);
	V3Stats::addStat("Tracing, Unique trace codes", m_statUniqCodes);
    }
};

//######################################################################
// Trace class functions

void V3Trace::traceAll(AstNetlist* nodep) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    TraceVisitor visitor (nodep);
}
