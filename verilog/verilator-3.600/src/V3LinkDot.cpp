// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Resolve module/signal name references
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
// LinkDot TRANSFORMATIONS:
//	Top-down traversal
//	    Cells:
//		Make graph of cell hiearchy
//	    Var/Funcs's:
//		Collect all names into symtable under appropriate cell
//	Top-down traversal
//	    VarXRef/Func's:
//		Find appropriate named cell and link to var they reference
//*************************************************************************

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <map>
#include <algorithm>
#include <vector>

#include "V3Global.h"
#include "V3LinkDot.h"
#include "V3SymTable.h"
#include "V3Graph.h"
#include "V3Ast.h"

//######################################################################

class LinkDotGraph : public V3Graph {
public:
    LinkDotGraph() {}
    virtual ~LinkDotGraph() {}
    virtual string dotRankDir() { return "LR"; }
};

class LinkDotBaseVertex : public V3GraphVertex {
    typedef std::map<string,LinkDotBaseVertex*> NameVtxMap;
    // A point in the hierarchy, either inlined or real
    string	m_symPrefix;	// String to prefix symbols with
    NameVtxMap	m_nameToVtxMap;	// Lookup of name -> to vertexes
public:
    LinkDotBaseVertex(V3Graph* graphp, const string& symPrefix)
	: V3GraphVertex(graphp), m_symPrefix(symPrefix) {}
    virtual ~LinkDotBaseVertex() {}
    virtual string modName() const = 0;
    virtual string cellName() const = 0;
    virtual V3SymTable& syms() = 0;
    string symPrefix() const { return m_symPrefix; }
    void insertSubcellName(const string& name, LinkDotBaseVertex* toVertexp) {
	m_nameToVtxMap.insert(make_pair(name,toVertexp));
    }
    LinkDotBaseVertex* findSubcell(const string& name) {
	// Find a vertex under this one by name.
	// We could walk the edge top() list, but that would be O(n) for large lists of cells
	NameVtxMap::iterator iter = m_nameToVtxMap.find(name);
	if (iter == m_nameToVtxMap.end()) return NULL;
	else return iter->second;
    }
};

class LinkDotCellVertex : public LinkDotBaseVertex {
    // A real point in the hierarchy, corresponding to a instantiated module
    AstModule*	m_modp;		// Module 
    AstCell*	m_cellp;	// Cell creating this vertex **NULL AT TOP**
    V3SymTable  m_syms;		// Symbol table of variable/task names for global lookup
public:
    LinkDotCellVertex(V3Graph* graphp, AstCell* nodep)
	: LinkDotBaseVertex(graphp, ""), m_modp(nodep->modp()), m_cellp(nodep) {}
    LinkDotCellVertex(V3Graph* graphp, AstModule* nodep)
	: LinkDotBaseVertex(graphp, ""), m_modp(nodep), m_cellp(NULL) {}
    virtual ~LinkDotCellVertex() {}
    AstModule* modp() const { return m_modp; }   // May be NULL
    AstCell* cellp() const { return m_cellp; }   // Is NULL at TOP
    virtual V3SymTable& syms() { return m_syms; }
    // We need to use origName as parameters may have renamed the modname
    virtual string modName() const { return (modp() ? modp()->origName() : "*NULL*"); }
    virtual string cellName() const { return (cellp() ? cellp()->origName() : "*NULL*"); }
    virtual string name() const { return (string)("C:")+cellName()+" M:"+modName(); }
};

class LinkDotInlineVertex : public LinkDotBaseVertex {
    // A fake point in the hierarchy, corresponding to a inlined module
    // This refrences to another vertex, and eventually resolves to a module with a prefix
    string	m_basename;		// Name with dotteds stripped
    AstCellInline* m_cellInlinep; 	// Inlined cell
    LinkDotCellVertex* m_symVxp;	// Above cell so we can find real symbol table
    //					// (Could walk graph to find it, but that's much slower.)
public:
    LinkDotInlineVertex(V3Graph* graphp, AstCellInline* nodep, LinkDotCellVertex* symVxp,
			const string& basename)
	: LinkDotBaseVertex(graphp, nodep->name()+"__DOT__")
	, m_basename(basename), m_cellInlinep(nodep), m_symVxp(symVxp) {}
    virtual ~LinkDotInlineVertex() {}
    AstCellInline* cellInlinep() const { return m_cellInlinep; }
    // Search up through tree to find the real symbol table.
    virtual V3SymTable& syms() { return m_symVxp->syms(); }
    virtual string modName() const { return cellInlinep()->origModName(); }
    virtual string cellName() const { return m_basename; }
    virtual string name() const { return (string)("INL C:")+cellName()+" M:"+modName()+" P:"+symPrefix(); }
    virtual string dotColor() const { return "yellow"; }
};

//######################################################################
// LinkDot state, as a visitor of each AstNode

class LinkDotState {
private:
    // NODE STATE
    // Cleared on Netlist
    //  AstModule::userp()	-> LinkDotCellVertex*.  Last cell that uses this module
    //  AstVarScope::user2p()	-> AstVarScope*.  Base alias for this signal

    // TYPES
    typedef std::multimap<string,LinkDotCellVertex*> NameScopeMap;
    // MEMBERS
    LinkDotGraph	m_graph;		// Graph of hiearchy
    NameScopeMap	m_nameScopeMap;		// Hash of scope referenced by textual name
    bool		m_forScopeCreation;	// Remove VarXRefs for V3Scope
public:
    static int debug() { return V3Error::debugDefault(); }
//    static int debug() { return 9; }

    // CONSTRUCTORS
    LinkDotState(bool forScopeCreation) {
	UINFO(4,__FUNCTION__<<": "<<endl);
	//VV*****  We reset all userp() on each netlist!!!
	AstNode::userClearTree();
	AstNode::user2ClearTree();
	m_forScopeCreation = forScopeCreation;
    }
    ~LinkDotState() {}
    
    // ACCESSORS
    bool forScopeCreation() const { return m_forScopeCreation; }

    // METHODS
    LinkDotCellVertex* insertTopCell(AstModule* nodep, const string& scopename) {
	UINFO(9,"      INSERTcell "<<scopename<<" "<<nodep<<endl);
	LinkDotCellVertex* vxp = new LinkDotCellVertex(&m_graph, nodep);
	nodep->userp(vxp);
	if (forScopeCreation()) m_nameScopeMap.insert(make_pair(scopename, vxp));
	return vxp;
    }
    LinkDotCellVertex* insertCell(LinkDotBaseVertex* abovep, LinkDotCellVertex* cellVxp,
				  AstCell* nodep, const string& scopename) {
	UINFO(9,"      INSERTcell "<<scopename<<" "<<nodep<<endl);
	LinkDotCellVertex* vxp = new LinkDotCellVertex(&m_graph, nodep);
	if (nodep->modp()) nodep->modp()->userp(vxp);
	new V3GraphEdge(&m_graph, abovep, vxp, 1, false);
	abovep->insertSubcellName(nodep->origName(), vxp);
	if (abovep != cellVxp) {
	    // If it's foo_DOT_bar, we need to be able to find it under that too.
	    cellVxp->insertSubcellName(nodep->name(), vxp);
	}
	if (forScopeCreation()) m_nameScopeMap.insert(make_pair(scopename, vxp));
	return vxp;
    }
    LinkDotInlineVertex* insertInline(LinkDotBaseVertex* abovep, LinkDotCellVertex* cellVxp,
				      AstCellInline* nodep, const string& basename) {
	UINFO(9,"      INSERTcinl "<<nodep<<endl);
	LinkDotInlineVertex* vxp = new LinkDotInlineVertex(&m_graph, nodep, cellVxp, basename);
	new V3GraphEdge(&m_graph, abovep, vxp, 1, false);
	abovep->insertSubcellName(basename, vxp);
	if (abovep != cellVxp) {
	    // If it's foo_DOT_bar, we need to be able to find it under that too.
	    cellVxp->insertSubcellName(nodep->name(), vxp);
	}
	return vxp;
    }
    void insertSym(LinkDotCellVertex* abovep, const string& name, AstNode* nodep) {
	UINFO(9,"      INSERTsym "<<name<<" "<<nodep<<endl);
	abovep->syms().insert(name, nodep);
    }
    bool existsModScope(AstModule* nodep) {
	return nodep->userp()!=NULL;
    }
    LinkDotCellVertex* findModScope(AstModule* nodep) {
	LinkDotCellVertex* vxp = (LinkDotCellVertex*)(nodep->userp());
	if (!vxp) nodep->v3fatalSrc("Module never assigned a vertex");
	return vxp;
    }
    LinkDotCellVertex* findScope(AstScope* nodep) {
	NameScopeMap::iterator iter = m_nameScopeMap.find(nodep->name());
	if (iter == m_nameScopeMap.end()) {
	    nodep->v3fatalSrc("Scope never assigned a vertex");
	}
	return iter->second;
    }
    void dump() {
	if (debug()>=6) m_graph.dumpDotFilePrefixed("linkdot");
    }
private:
    LinkDotBaseVertex* parentOfCell(LinkDotBaseVertex* lowerVxp) {
	for (V3GraphEdge* edgep = lowerVxp->inBeginp(); edgep; edgep=edgep->inNextp()) {
	    LinkDotBaseVertex* fromVxp = dynamic_cast<LinkDotBaseVertex*>(edgep->fromp());
	    return fromVxp;
	}
	return NULL;
    }
public:
    LinkDotBaseVertex* findDotted(LinkDotBaseVertex* cellVxp, const string& dotname, string& baddot) {
	// Given a dotted hiearchy name, return where in scope it is
	// Note when dotname=="" we just fall through and return cellVxp
	UINFO(8,"    dottedFind "<<dotname<<endl);
	bool firstId = true;
	string leftname = dotname;
	while (leftname != "") {  // foreach dotted part of xref name
	    string::size_type pos;
	    string ident;
	    if ((pos = leftname.find(".")) != string::npos) {
		ident = leftname.substr(0,pos);
		leftname = leftname.substr(pos+1);
	    } else {
		ident = leftname;
		leftname = "";
	    }
	    baddot = ident;   // So user can see where they botched it
	    UINFO(8,"         id "<<ident<<" left "<<leftname<<" at "<<cellVxp<<endl);
	    // Spec says; Look at exiting module (cellnames then modname),
	    // then look up (inst name or modname)
	    if (firstId) {
		// Check this module - subcellnames
		if (LinkDotBaseVertex* findVxp = cellVxp->findSubcell(ident)) {
		    cellVxp = findVxp;
		}
		// Check this module - cur modname
		else if (cellVxp->modName() == ident) {}
		// Check this module - cur cellname
		else if (cellVxp->cellName() == ident) {}
		// Move up and check cellname + modname
		else {
		    while (cellVxp) {
			cellVxp = parentOfCell(cellVxp);
			if (cellVxp) {
			    UINFO(9,"\t\tUp to "<<cellVxp<<endl);
			    if (cellVxp->modName() == ident
				|| cellVxp->cellName() == ident) {
				break;
			    }
			    else if (LinkDotBaseVertex* findVxp = cellVxp->findSubcell(ident)) {
				cellVxp = findVxp;
				break;
			    }
			}
		    }
		    if (!cellVxp) return NULL;  // Not found
		}
	    } else { // Searching for middle submodule, must be a cell name
		if (LinkDotBaseVertex* findVxp = cellVxp->findSubcell(ident)) {
		    cellVxp = findVxp;
		} else {
		    return NULL;  // Not found
		}
	    }
	    firstId = false;
	}
	return cellVxp;
    }

    AstNode* findSym(LinkDotBaseVertex* cellVxp, const string& dotname, string& baddot) {
	// Find symbol in given point in hierarchy
	// For simplicity cellVxp may be passed NULL result from findDotted
	if (!cellVxp) return NULL;
	UINFO(8,"\t\tfindSym "<<dotname
	      <<((cellVxp->symPrefix()=="") ? "" : " as ")
	      <<((cellVxp->symPrefix()=="") ? "" : cellVxp->symPrefix()+dotname)
	      <<"  at  "<<cellVxp
	      <<endl);
	AstNode* nodep = cellVxp->syms().findIdName(cellVxp->symPrefix() + dotname);  // Might be NULL
	if (!nodep) baddot = dotname;
	return nodep;
    }
};

//======================================================================

class LinkDotFindVisitor : public AstNVisitor {
private:
    // STATE
    LinkDotState*	m_statep;	// State to pass between visitors, including symbol table
    LinkDotCellVertex*	m_cellVxp;	// Vertex for current module
    LinkDotBaseVertex*	m_inlineVxp;	// Vertex for current module, possibly a fake inlined one
    string		m_scope;	// Scope text
    AstBegin*		m_beginp;	// Current Begin/end block
    int debug() { return LinkDotState::debug(); }

    // VISITs
    virtual void visit(AstNetlist* nodep, AstNUser*) {
	// The first module in the list is always the top module (sorted before this is called).
	// This may not be the module with isTop() set, as early in the steps,
	// wrapTop may have not been created yet.
	AstModule* topmodp = nodep->modulesp();
	if (!topmodp) nodep->v3fatalSrc("No top level module");
	UINFO(8,"Top Module: "<<topmodp<<endl);
	m_scope = "TOP";
	m_cellVxp = m_statep->insertTopCell(topmodp, m_scope);
	m_inlineVxp = m_cellVxp;
	{
	    topmodp->accept(*this);
	}
	m_scope = "";
	m_cellVxp = NULL;
	m_inlineVxp = m_cellVxp;
    }
    virtual void visit(AstModule* nodep, AstNUser*) {
	UINFO(8,"   "<<nodep<<endl);
	if (!m_cellVxp) {
	    // Will be optimized away later
	    UINFO(5, "Module not under any CELL or top - dead module\n");
	} else {
	    nodep->iterateChildren(*this);
	}
    }
    virtual void visit(AstScope* nodep, AstNUser*) {
	if (!m_statep->forScopeCreation()) v3fatalSrc("Scopes should only exist right after V3Scope");
	// Ignored.  Processed in next step
    }
    virtual void visit(AstCell* nodep, AstNUser*) {
	UINFO(5,"   CELL under "<<m_scope<<" is "<<nodep<<endl);
	// Recurse in, preserving state
	string oldscope = m_scope;
	LinkDotCellVertex* oldVxp = m_cellVxp;
	// Where do we add it?
	LinkDotBaseVertex* aboveVxp = m_cellVxp;
	string origname = nodep->prettyName();
	string::size_type pos;
	if ((pos = origname.rfind(".")) != string::npos) {
	    // Flattened, find what CellInline it should live under
	    string scope = origname.substr(0,pos);
	    string baddot;
	    aboveVxp = m_statep->findDotted(aboveVxp, scope, baddot);
	    if (!aboveVxp) nodep->v3fatalSrc("Can't find cell insertion point at '"<<baddot<<"' in: "<<nodep->prettyName());
	}
	{
	    m_scope = m_scope+"."+nodep->name();
	    m_cellVxp = m_statep->insertCell(aboveVxp, m_cellVxp, nodep, m_scope);
	    m_inlineVxp = m_cellVxp;
	    if (nodep->modp()) nodep->modp()->accept(*this);
	}
	m_scope = oldscope;
	m_cellVxp = oldVxp;
	m_inlineVxp = m_cellVxp;
    }
    virtual void visit(AstCellInline* nodep, AstNUser*) {
	UINFO(5,"   CELLINLINE under "<<m_scope<<" is "<<nodep<<endl);
	LinkDotBaseVertex* aboveVxp = m_inlineVxp;
	// If baz__DOT__foo__DOT__bar, we need to find baz__DOT__foo and add bar to it.
	string dottedname = nodep->name();
	string::size_type pos;
	if ((pos=dottedname.rfind("__DOT__")) != string::npos) {
	    string dotted = dottedname.substr(0, pos);
	    string ident  = dottedname.substr(pos+strlen("__DOT__"));
	    string baddot;
	    aboveVxp = m_statep->findDotted(aboveVxp, dotted, baddot);
	    if (!aboveVxp) nodep->v3fatalSrc("Can't find cellinline insertion point at '"<<baddot<<"' in: "<<nodep->prettyName());
	    m_statep->insertInline(aboveVxp, m_cellVxp, nodep, ident);
	} else {  // No __DOT__, just directly underneath
	    m_statep->insertInline(aboveVxp, m_cellVxp, nodep, nodep->name());
	}
    }
    virtual void visit(AstBegin* nodep, AstNUser*) {
	UINFO(5,"   "<<nodep<<endl);
	// We don't pickup variables, but do need to find cells
	AstBegin* oldbegin = m_beginp;
	m_beginp = nodep;
	nodep->iterateChildren(*this);
	m_beginp = oldbegin;
    }
    virtual void visit(AstVar* nodep, AstNUser*) {
	if (!m_statep->forScopeCreation()
	    && !m_beginp	// For now, we don't support xrefs into begin blocks
	    && !nodep->isFuncLocal()) {
	    m_statep->insertSym(m_cellVxp, nodep->name(), nodep);
	}
    }
    virtual void visit(AstNodeFTask* nodep, AstNUser*) {
	m_statep->insertSym(m_cellVxp, nodep->name(), nodep);
	// No recursion, we don't want to pick up variables
    }
    virtual void visit(AstCFunc* nodep, AstNUser*) {
	// Ignore all AstVars under functions
    }

    // For speed, don't recurse things that can't have cells
    virtual void visit(AstNodeStmt*, AstNUser*) {}
    virtual void visit(AstNodeMath*, AstNUser*) {}
    virtual void visit(AstNode* nodep, AstNUser*) {
	// Default: Just iterate
	nodep->iterateChildren(*this);
    }

public:
    // CONSTUCTORS
    LinkDotFindVisitor(AstNetlist* rootp, LinkDotState* statep) {
	UINFO(4,__FUNCTION__<<": "<<endl);
	m_cellVxp = NULL;
	m_statep = statep;
	m_beginp = NULL;
	//
	rootp->accept(*this);
    }
    virtual ~LinkDotFindVisitor() {}
};

//======================================================================

class LinkDotScopeVisitor : public AstNVisitor {
private:
    // STATE
    LinkDotState*	m_statep;	// State to pass between visitors, including symbol table
    LinkDotCellVertex*	m_cellVxp;	// Vertex for current module
    int debug() { return LinkDotState::debug(); }

    // VISITs
    virtual void visit(AstScope* nodep, AstNUser*) {
	UINFO(8,"  SCOPE "<<nodep<<endl);
	if (!m_statep->forScopeCreation()) v3fatalSrc("Scopes should only exist right after V3Scope");
	// Using the CELL names, we created all hiearchy.  We now need to match this Scope
	// up with the hiearchy created by the CELL names.
	m_cellVxp = m_statep->findScope(nodep);
	nodep->iterateChildren(*this);
	m_cellVxp = NULL;
    }
    virtual void visit(AstVarScope* nodep, AstNUser*) {
	if (!nodep->varp()->isFuncLocal()) {
	    m_statep->insertSym(m_cellVxp, nodep->varp()->name(), nodep);
	}
    }
    virtual void visit(AstNodeFTask* nodep, AstNUser*) {
	m_statep->insertSym(m_cellVxp, nodep->name(), nodep);
	// No recursion, we don't want to pick up variables
    }
    virtual void visit(AstAssignAlias* nodep, AstNUser*) {
	// Track aliases created by V3Inline; if we get a VARXREF(aliased_from)
	// we'll need to replace it with a VARXREF(aliased_to)
	if (m_statep->forScopeCreation()) {
	    if (debug()>=9) nodep->dumpTree(cout,"-\t\t\t\talias: ");
	    AstVarScope* fromVscp = nodep->lhsp()->castVarRef()->varScopep();
	    AstVarScope* toVscp   = nodep->rhsp()->castVarRef()->varScopep();
	    if (!fromVscp || !toVscp) nodep->v3fatalSrc("Bad alias scopes");
	    fromVscp->user2p(toVscp);
	}
	nodep->iterateChildren(*this);
    }
    // For speed, don't recurse things that can't have scope
    virtual void visit(AstCell*, AstNUser*) {}
    virtual void visit(AstVar*, AstNUser*) {}
    virtual void visit(AstNodeStmt*, AstNUser*) {}
    virtual void visit(AstNodeMath*, AstNUser*) {}
    virtual void visit(AstNode* nodep, AstNUser*) {
	// Default: Just iterate
	nodep->iterateChildren(*this);
    }

public:
    // CONSTUCTORS
    LinkDotScopeVisitor(AstNetlist* rootp, LinkDotState* statep) {
	UINFO(4,__FUNCTION__<<": "<<endl);
	m_cellVxp = NULL;
	m_statep = statep;
	//
	rootp->accept(*this);
    }
    virtual ~LinkDotScopeVisitor() {}
};

//======================================================================

class LinkDotResolveVisitor : public AstNVisitor {
private:
    // STATE
    LinkDotState*	m_statep;	// State, including dotted symbol table
    LinkDotCellVertex*	m_cellVxp;	// Vertex for current module
    int debug() { return LinkDotState::debug(); }

    // METHODS

    // VISITs
    virtual void visit(AstModule* nodep, AstNUser*) {
	UINFO(8,"  "<<nodep<<endl);
	if (!m_statep->existsModScope(nodep)) {
	    UINFO(5,"Dead module for "<<nodep<<endl);
	    m_cellVxp = NULL;
	} else {
	    m_cellVxp = m_statep->findModScope(nodep);
	}
	nodep->iterateChildren(*this);
	m_cellVxp = NULL;
    }
    virtual void visit(AstScope* nodep, AstNUser*) {
	UINFO(8,"   "<<nodep<<endl);
	m_cellVxp = m_statep->findScope(nodep);
	nodep->iterateChildren(*this);
	m_cellVxp = NULL;
    }
    virtual void visit(AstCellInline* nodep, AstNUser*) {
	if (m_statep->forScopeCreation()) {
	    nodep->unlinkFrBack(); pushDeletep(nodep); nodep=NULL;
	}
    }
    virtual void visit(AstVarXRef* nodep, AstNUser*) {
	// VarRef: Resolve its reference
	// We always link even if varp() is set, because the module we choose may change
	// due to creating new modules, flattening, etc.
	UINFO(8,"     "<<nodep<<endl);
	if (!m_cellVxp) {
	    UINFO(9,"Dead module for "<<nodep<<endl);
	    nodep->varp(NULL);  // Module that is not in hiearchy.  We'll be dead code eliminating it later.
	} else {
	    string baddot;
	    LinkDotBaseVertex* dotVxp = m_cellVxp;  // Start search at current scope
	    if (nodep->inlinedDots()!="") {  // Correct for current scope
		dotVxp = m_statep->findDotted(dotVxp, nodep->inlinedDots(), baddot);
		if (!dotVxp) nodep->v3fatalSrc("Couldn't resolve inlined scope '"<<baddot<<"' in: "<<nodep->inlinedDots());
	    }
	    dotVxp = m_statep->findDotted(dotVxp, nodep->dotted(), baddot); // Maybe NULL
	    if (!m_statep->forScopeCreation()) {
		AstVar* varp = (m_statep->findSym(dotVxp, nodep->name(), baddot)
				->castVar());  // maybe NULL
		nodep->varp(varp);
		UINFO(7,"         Resolved "<<nodep<<endl);  // Also prints varp
		if (!nodep->varp()) {
		    nodep->v3error("Can't find definition of '"<<baddot<<"' in dotted signal: "<<nodep->dotted()+"."+nodep->prettyName());
		}
	    } else {
		string baddot;
		AstVarScope* vscp = (m_statep->findSym(dotVxp, nodep->name(), baddot)
				     ->castVarScope());  // maybe NULL
		if (!vscp) {
		    nodep->v3error("Can't find varpin scope of '"<<baddot<<"' in dotted signal: "<<nodep->dotted()+"."+nodep->prettyName());
		} else {
		    while (vscp->user2p()) {  // If V3Inline aliased it, pick up the new signal
			UINFO(7,"         Resolved pre-alias "<<vscp<<endl);  // Also prints taskp
			vscp = vscp->user2p()->castNode()->castVarScope();
		    }
		    // Convert the VarXRef to a VarRef, so we don't need later optimizations to deal with VarXRef.
		    nodep->varp(vscp->varp());
		    nodep->varScopep(vscp);
		    UINFO(7,"         Resolved "<<nodep<<endl);  // Also prints taskp
		    AstVarRef* newvscp = new AstVarRef(nodep->fileline(), vscp, nodep->lvalue());
		    nodep->replaceWith(newvscp);
		    nodep->deleteTree(); nodep=NULL;
		}
	    }
	}
    }
    virtual void visit(AstNodeFTaskRef* nodep, AstNUser*) {
	UINFO(8,"     "<<nodep<<endl);
	if (!m_cellVxp) {
	    UINFO(9,"Dead module for "<<nodep<<endl);
	    nodep->taskp(NULL);  // Module that is not in hiearchy.  We'll be dead code eliminating it later.
	} else {
	    string baddot;
	    LinkDotBaseVertex* dotVxp = m_cellVxp;  // Start search at current scope
	    if (nodep->inlinedDots()!="") {  // Correct for current scope
		dotVxp = m_statep->findDotted(dotVxp, nodep->inlinedDots(), baddot);
		if (!dotVxp) nodep->v3fatalSrc("Couldn't resolve inlined scope '"<<baddot<<"' in: "<<nodep->inlinedDots());
	    }
	    dotVxp = m_statep->findDotted(dotVxp, nodep->dotted(), baddot); // Maybe NULL

	    AstNodeFTask* taskp = (m_statep->findSym(dotVxp, nodep->name(), baddot)
				   ->castNodeFTask()); // maybe NULL
	    nodep->taskp(taskp);
	    UINFO(7,"         Resolved "<<nodep<<endl);  // Also prints taskp
	    if (!nodep->taskp()) {
		nodep->v3error("Can't find definition of '"<<baddot<<"' in dotted task/function: "<<nodep->dotted()+"."+nodep->prettyName());
	    }
	}
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstNode* nodep, AstNUser*) {
	// Default: Just iterate
	nodep->iterateChildren(*this);
    }
public:
    // CONSTUCTORS
    LinkDotResolveVisitor(AstNetlist* rootp, LinkDotState* statep) {
	UINFO(4,__FUNCTION__<<": "<<endl);
	m_statep = statep;
	m_cellVxp = NULL;
	//
	rootp->accept(*this);
    }
    virtual ~LinkDotResolveVisitor() {}
};

//######################################################################
// Link class functions

void V3LinkDot::linkDot(AstNetlist* rootp) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    if (LinkDotState::debug()>=5) v3Global.rootp()->dumpTreeFile(v3Global.debugFilename("prelinkdot.tree"));
    LinkDotState state (false);
    LinkDotFindVisitor visitor(rootp,&state);
    state.dump();
    LinkDotResolveVisitor visitorb(rootp,&state);
}

void V3LinkDot::linkDotScope(AstNetlist* rootp) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    if (LinkDotState::debug()>=5) v3Global.rootp()->dumpTreeFile(v3Global.debugFilename("prelinkdot.tree"));
    LinkDotState state (true);
    LinkDotFindVisitor visitor(rootp,&state);
    // Process AstScope's.  This needs to be separate pass after whole hiearchy graph created.
    LinkDotScopeVisitor visitors(rootp,&state);
    state.dump();
    LinkDotResolveVisitor visitorb(rootp,&state);
}
