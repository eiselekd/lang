// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Replicate modules for parameterization
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
// PARAM TRANSFORMATIONS:
//   Top down traversal:
//	For each cell:
//	    If parameterized,
//		Determine all parameter widths, constant values
//	    	Clone module cell calls, renaming with __{par1}_{par2}_...
//		Substitute constants for cell's module's parameters
//		Relink pins and cell to point to new module
//
//*************************************************************************

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <map>
#include <vector>

#include "V3Global.h"
#include "V3Param.h"
#include "V3Read.h"
#include "V3Ast.h"
#include "V3Case.h"
#include "V3Const.h"
#include "V3Width.h"
#include "V3Signed.h"
#include "V3Unroll.h"

//######################################################################
// Param state, as a visitor of each AstNode

class ParamVisitor : public AstNVisitor {
private:
    // NODE STATE
    //	 AstModule::user4()	// bool	  True if parameters numbered
    //   AstVar::user4()	// int    Global parameter number (for naming new module)
    // STATE
    typedef std::map<AstVar*,AstVar*> VarCloneMap;
    struct ModInfo {
	AstModule*	m_modp;		// Module with specified name
	VarCloneMap	m_cloneMap;	// Map of old-varp -> new cloned varp
	ModInfo(AstModule* modp) { m_modp=modp; }
    };
    typedef std::map<string,ModInfo> ModNameMap;
    ModNameMap	m_modNameMap;	// Hash of created module flavors by name

    typedef std::map<string,string> LongMap;
    LongMap	m_longMap;	// Hash of very long names to unique identity number
    int		m_longId;

    //int debug() { return 9; }

    // METHODS
    void makeSmallNames(AstModule* modp) {
	vector<int> usedLetter; usedLetter.resize(256);
	// Pass 1, assign first letter to each gparam's name
	for (AstNode* stmtp = modp->stmtsp(); stmtp; stmtp=stmtp->nextp()) {
	    if (AstVar* varp = stmtp->castVar()) {
		if (varp->isGParam()) {
		    char ch = varp->name()[0];
		    ch = toupper(ch); if (ch<'A' || ch>'Z') ch='Z';
		    varp->user4(usedLetter[ch]*256 + ch);
		    usedLetter[ch]++;
		}
	    }
	}
    }
    string paramSmallName(AstModule* modp, AstVar* varp) {
	if (!varp->user4()) {
	    makeSmallNames(modp);
	}
	int index = varp->user4()/256;
	char ch   = varp->user4()&255;
	string st = cvtToStr(ch);
	while (index) {
	    st += cvtToStr(char((index%26)+'A'));
	    index /= 26;
	}
	return st;
    }
    void relinkPins(VarCloneMap* clonemapp, AstPin* startpinp) {
	for (AstPin* pinp = startpinp; pinp; pinp=pinp->nextp()->castPin()) {
	    if (!pinp->modVarp()) pinp->v3fatalSrc("Not linked?\n");
	    // Find it in the clone structure
	    //UINFO(8,"Clone find 0x"<<hex<<(uint32_t)pinp->modVarp()<<endl);
	    VarCloneMap::iterator cloneiter = clonemapp->find(pinp->modVarp());
	    UASSERT(cloneiter != clonemapp->end(), "Couldn't find pin in clone list");
	    pinp->modVarp(cloneiter->second);
	}
    }

    // VISITORS
    virtual void visit(AstNetlist* nodep, AstNUser*) {
	AstNode::user4ClearTree();
	// Modules must be done in top-down-order
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstModule* nodep, AstNUser*) {
	UINFO(4," MOD   "<<nodep<<endl);
	nodep->iterateChildren(*this);
    }
    virtual void visit(AstCell* nodep, AstNUser*);

    // Generate Statements
    virtual void visit(AstGenerate* nodep, AstNUser*) {
	if (debug()>=9) nodep->dumpTree(cout,"-genin: ");
	nodep->iterateChildren(*this);
	// After expanding the generate, all statements under it can be moved
	// up, and the generate block deleted as it's not relevant
	AstNode* stmtsp = nodep->stmtsp()->unlinkFrBackWithNext();
	nodep->replaceWith(stmtsp);
	nodep->deleteTree(); nodep=NULL;
	if (debug()>=9) stmtsp->dumpTree(cout,"-genout: ");
    }
    virtual void visit(AstGenIf* nodep, AstNUser*) {
	V3Width::widthParams(nodep);  // Param typed widthing will NOT recurse the body
	V3Signed::signedParams(nodep);
	V3Const::constifyParam(nodep->condp());
	if (AstConst* constp = nodep->condp()->castConst()) {
	    AstNode* keepp = (constp->isZero()
			      ? nodep->elsesp()
			      : nodep->ifsp());
	    if (keepp) {
		keepp->unlinkFrBackWithNext();
		nodep->replaceWith(keepp);
	    } else {
		nodep->unlinkFrBack();
	    }
	    nodep->deleteTree(); nodep=NULL;
	} else {
	    nodep->condp()->v3error("Generate If condition must evaluate to constant");
	}
    }
    virtual void visit(AstGenFor* nodep, AstNUser*) {
	// We parse a very limited form of FOR, so we don't need to do a full
	// simulation to unroll the loop
	V3Width::widthParams(nodep);  // Param typed widthing will NOT recurse the body
	V3Signed::signedParams(nodep);
	V3Unroll::unrollGen(nodep); nodep=NULL;
    }
    virtual void visit(AstGenCase* nodep, AstNUser*) {
	AstNode* keepp = NULL;
	V3Case::caseLint(nodep);
	V3Width::widthParams(nodep);  // Param typed widthing will NOT recurse the body
	V3Signed::signedParams(nodep);
	V3Const::constifyParam(nodep->exprp());
	AstConst* exprp = nodep->exprp()->castConst();
	// Constify
	for (AstCaseItem* itemp = nodep->itemsp(); itemp; itemp=itemp->nextp()->castCaseItem()) {
	    for (AstNode* ep = itemp->condsp(); ep; ) {
		AstNode* nextp = ep->nextp(); //May edit list
		V3Const::constifyParam(ep);
		ep = nextp;
	    }
	}
	// Item match
	for (AstCaseItem* itemp = nodep->itemsp(); itemp; itemp=itemp->nextp()->castCaseItem()) {
	    if (!itemp->isDefault()) {
		for (AstNode* ep = itemp->condsp(); ep; ep=ep->nextp()) {
		    if (AstConst* ccondp = ep->castConst()) {
			V3Number match (nodep->fileline(), 1);
			match.opEq(ccondp->num(), exprp->num());
			if (!keepp && match.isNeqZero()) {
			    keepp = itemp->bodysp();
			}
		    } else {
			itemp->v3error("Generate Case item does not evaluate to constant");
		    }
		}
	    }
	}
	// Else default match
	for (AstCaseItem* itemp = nodep->itemsp(); itemp; itemp=itemp->nextp()->castCaseItem()) {
	    if (itemp->isDefault()) {
		if (!keepp) keepp=itemp->bodysp();
	    }
	}
	// Replace
	if (keepp) {
	    keepp->unlinkFrBackWithNext();
	    nodep->replaceWith(keepp);
	}
	else nodep->unlinkFrBack();
	nodep->deleteTree(); nodep=NULL;
    }

    // Default: Just iterate
    virtual void visit(AstNode* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
    }

public:
    // CONSTUCTORS
    ParamVisitor(AstNetlist* nodep) {
	m_longId = 0;
	//
	nodep->accept(*this);
    }
    virtual ~ParamVisitor() {}
};

//----------------------------------------------------------------------
// VISITs

void ParamVisitor::visit(AstCell* nodep, AstNUser*) {
    // Cell: Check for parameters in the instantiation.
    if (!nodep->modp()) { nodep->dumpTree(cout,"error:"); nodep->v3fatalSrc("Not linked?"); }
    if (nodep->paramsp()) {	
	UINFO(4,"De-parameterize: "<<nodep<<endl);
	// Create new module name with _'s between the constants
	if (debug()>9) nodep->dumpTree(cout,"cell:\t");
	// Evaluate all module constants
	V3Const::constifyParam(nodep);
	// Make sure constification worked
	// Must be a separate loop, as constant conversion may have changed some pointers.
	//if (debug()) nodep->dumpTree(cout,"cel2:\t");
	string longname = nodep->modp()->name();
	longname += "_";
	if (debug()>8) nodep->paramsp()->dumpTreeAndNext(cout,"-cellparams:\t");
	for (AstPin* pinp = nodep->paramsp(); pinp; pinp=pinp->nextp()->castPin()) {
	    if (!pinp) nodep->v3fatalSrc("Non pin under cell params\n");
	    AstVar* modvarp = pinp->modVarp();
	    if (!modvarp) {
		pinp->v3error("Parameter not found in sub-module: Param "<<pinp->name()<<" of "<<nodep->prettyName());
	    } else if (!modvarp->isGParam()) {
		pinp->v3error("Attempted parameter setting of non-parameter: Param "<<pinp->name()<<" of "<<nodep->prettyName());
	    } else {
		AstConst* constp = pinp->exprp()->castConst();
		if (!constp) {
		    //if (debug()) pinp->dumpTree(cout,"error:");
		    pinp->v3error("Can't convert defparam value to constant: Param "<<pinp->name()<<" of "<<nodep->prettyName());
		} else {
		    longname += "_" + paramSmallName(nodep->modp(),pinp->modVarp())+constp->num().ascii(false);
		}
	    }
	}

	//
	// If the name is very long, we don't want to overwhelm the filename limit
	// We don't do this always, as it aids debugability to have intuitive naming.
	string newname = longname;
	if (longname.length()>30) {
	    LongMap::iterator iter = m_longMap.find(longname);
	    if (iter != m_longMap.end()) {
		newname = iter->second;
	    } else {
		newname = nodep->modp()->name();
		newname += "__pi"+cvtToStr(++m_longId);  // We use all upper case above, so lower here can't conflict
		m_longMap.insert(make_pair(longname, newname));
	    }
	}
	UINFO(4,"Name: "<<nodep->modp()->name()<<"->"<<longname<<"->"<<newname<<endl);

	//
	// Already made this flavor?
	AstModule* modp = NULL;
	ModNameMap::iterator iter = m_modNameMap.find(newname);
	if (iter != m_modNameMap.end()) modp = iter->second.m_modp;
	if (!modp) {
	    // Deep clone of new module
	    // Note all module internal variables will be re-linked to the new modules by clone
	    // However links outside the module (like on the upper cells) will not.
	    modp = nodep->modp()->cloneTree(false)->castModule();
	    modp->name(newname);
	    nodep->modp()->addNextHere(modp);  // Keep tree sorted by cell occurrences

	    m_modNameMap.insert(make_pair(modp->name(), ModInfo(modp)));
	    iter = m_modNameMap.find(newname);
	    VarCloneMap* clonemapp = &(iter->second.m_cloneMap);
	    UINFO(4,"     De-parameterize to new: "<<modp<<endl);

	    // Grab all I/O so we can remap our pins later
	    // Note we allow multiple users of a parameterized model, thus we need to stash this info.
	    for (AstNode* stmtp=modp->stmtsp(); stmtp; stmtp = stmtp->nextp()) {
		if (AstVar* varp = stmtp->castVar()) {
		    if (varp->isIO() || varp->isGParam()) {
			// Cloning saved a pointer to the new node for us, so just follow that link.
			AstVar* oldvarp = varp->clonep()->castVar();
			//UINFO(8,"Clone list 0x"<<hex<<(uint32_t)oldvarp<<" -> 0x"<<(uint32_t)varp<<endl);
			clonemapp->insert(make_pair(oldvarp, varp));
		    }
		}
	    }

	    // Relink parameter vars to the new module
	    relinkPins(clonemapp, nodep->paramsp());

	    // Assign parameters to the constants specified
	    for (AstPin* pinp = nodep->paramsp(); pinp; pinp=pinp->nextp()->castPin()) {
		AstVar* modvarp = pinp->modVarp();
		if (modvarp) {
		    AstConst* constp = pinp->exprp()->castConst();
		    // Remove any existing parameter
		    if (modvarp->initp()) modvarp->initp()->unlinkFrBack()->deleteTree();
		    // Set this parameter to value requested by cell
		    modvarp->initp(constp->cloneTree(false));
		}
	    }
	} else {
	    UINFO(4,"     De-parameterize to old: "<<modp<<endl);
	}

	// Have child use this module instead.
	nodep->modp(modp);
	nodep->modName(newname);

	// Delete the parameters from the cell; they're not relevant any longer.
	{
	    AstNRelinker delHandle;
	    nodep->paramsp()->unlinkFrBackWithNext(&delHandle);
	    delHandle.oldp()->deleteTree();
	}
	
	// We need to relink the pins to the new module
	VarCloneMap* clonemapp = &(iter->second.m_cloneMap);
	relinkPins(clonemapp, nodep->pinsp());

	UINFO(8,"     Done with "<<modp<<endl);
    }
}

//######################################################################
// Param class functions

void V3Param::param(AstNetlist* rootp) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    ParamVisitor visitor (rootp);
}
