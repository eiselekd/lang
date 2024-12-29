// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Constant folding
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
// CONST TRANSFORMATIONS:
//   Call on one node for PARAM values, or netlist for overall constant folding:
//	Bottom up traversal:
//	    Attempt to convert operands to constants
//	    If operands are constant, replace this node with constant.
//*************************************************************************

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <map>

#include "V3Global.h"
#include "V3Const.h"
#include "V3Read.h"
#include "V3Ast.h"
#include "V3Width.h"
#include "V3Signed.h"

//######################################################################
// Const state, as a visitor of each AstNode

class ConstVisitor : public AstNVisitor {
private:
    // STATE
    bool	m_params;	// If true, propogate parameterized and true numbers only
    bool	m_required;	// If true, must become a constant
    bool	m_wremove;	// Inside scope, no assignw removal
    bool	m_warn;		// Output warnings
    bool	m_cpp;		// C++ conversions only
    AstModule*	m_modp;		// Current module	
    AstNode*	m_scopep;	// Current scope
    //int debug() { return 9; }

    // METHODS
    bool operandConst (AstNode* nodep) {
	return (nodep->castConst());
    }
    bool operandAsvConst (AstNode* nodep) {
	// BIASV(CONST, BIASV(CONST,...)) -> BIASV( BIASV_CONSTED(a,b), ...)
	AstNodeBiComAsv* bnodep = nodep->castNodeBiComAsv();
	if (!bnodep) return false;
	if (!bnodep->lhsp()->castConst()) return false;
	AstNodeBiComAsv* rnodep = bnodep->rhsp()->castNodeBiComAsv();
	if (!rnodep) return false;
	if (rnodep->type() != bnodep->type()) return false;
	if (rnodep->width() != bnodep->width()) return false;
	if (rnodep->lhsp()->width() != bnodep->lhsp()->width()) return false;
	if (!rnodep->lhsp()->castConst()) return false;
	return true;
    }
    bool operandHugeShiftL(AstNodeBiop* nodep) {
	return (nodep->rhsp()->castConst()
		&& nodep->rhsp()->castConst()->asInt() >= (uint32_t)(nodep->width()));
    }
    bool operandHugeShiftR(AstNodeBiop* nodep) {
	return (nodep->rhsp()->castConst()
		&& nodep->rhsp()->castConst()->asInt() >= (uint32_t)(nodep->lhsp()->width()));
    }
    bool operandIsTwo(AstNode* nodep) {
	return (nodep->castConst()
		&& nodep->width() <= VL_QUADSIZE 
		&& nodep->castConst()->asQuad()==2);
    }
    bool operandIsTwostate(AstNode* nodep) {
	return (nodep->castConst()
		&& !nodep->castConst()->num().isFourState());
    }
    bool operandShiftOp(AstNodeBiop* nodep) {
	if (!nodep->rhsp()->castConst()) return false;
	AstNodeBiop* lhsp = nodep->lhsp()->castNodeBiop();
	if (!lhsp || !(lhsp->castAnd()||lhsp->castOr()||lhsp->castXor())) return false;
	if (nodep->width()!=lhsp->width()) return false;
	if (nodep->width()!=lhsp->lhsp()->width()) return false;
	if (nodep->width()!=lhsp->rhsp()->width()) return false;
	return true;
    }
    bool operandShiftShift(AstNodeBiop* nodep) {
	// We could add a AND though.
	AstNodeBiop* lhsp = nodep->lhsp()->castNodeBiop();
	if (!lhsp || !(lhsp->castShiftL()||lhsp->castShiftR())) return false;
	// We can only get rid of a<<b>>c or a<<b<<c, with constant b & c
	// because bits may be masked in that process, or (b+c) may exceed the word width.
	if (!(nodep->rhsp()->castConst() && lhsp->rhsp()->castConst())) return false;
	if (nodep->width()!=lhsp->width()) return false;
	if (nodep->width()!=lhsp->lhsp()->width()) return false;
	return true;
    }
    bool operandWordOOB(AstWordSel* nodep) {
	// V3Expand may make a arraysel that exceeds the bounds of the array
	// It was an expression, then got constified.  In reality, the WordSel
	// must be wrapped in a Cond, that will be false.
	return (nodep->rhsp()->castConst()
		&& nodep->fromp()->castNodeVarRef()
		&& !nodep->fromp()->castNodeVarRef()->lvalue()
		&& ((int)(nodep->rhsp()->castConst()->asInt())
		    >= nodep->fromp()->castNodeVarRef()->varp()->widthWords()));
    }
    bool operandSelFull(AstSel* nodep) {
	return (nodep->lsbp()->castConst()
		&& nodep->widthp()->castConst()
		&& nodep->lsbConst()==0
		&& (int)nodep->widthConst()==nodep->fromp()->width()
		&& 1);
    }
    bool operandSelExtend(AstSel* nodep) {
	// A pattern created by []'s after offsets have been removed
	// SEL(EXTEND(any,width,...),(width-1),0) -> ...
	// Since select's return unsigned, this is always an extend
	AstExtend* extendp = nodep->fromp()->castExtend();
	return (!m_cpp
		&& extendp
		&& nodep->lsbp()->castConst()
		&& nodep->widthp()->castConst()
		&& nodep->lsbConst()==0
		&& (int)nodep->widthConst()==extendp->lhsp()->width()
		);
    }

    AstNode* afterComment(AstNode* nodep) {
	// Ignore comments, such as to determine if a AstIf is empty.
	// nodep may be null, if so return null.
	while (nodep && nodep->castComment()) { nodep = nodep->nextp(); }
	return nodep;
    }

    // Extraction checks
    bool warnSelect(AstSel* nodep) {
	AstNode* basefromp = AstArraySel::baseFromp(nodep->fromp());
	if (AstNodeVarRef* varrefp = basefromp->castNodeVarRef()) {
	    if (m_warn
		&& nodep->lsbp()->castConst()
		&& nodep->widthp()->castConst()
		&& (!varrefp->varp()->rangep() || varrefp->varp()->msb())  // else it's non-resolvable parameterized
		&& ( (  (nodep->msbConst() > varrefp->varp()->msb())
			|| (nodep->lsbConst() > varrefp->varp()->msb())))) {
		nodep->v3error("Selection index out of range: "
			       <<nodep->msbConst()<<":"<<nodep->lsbConst()
			       <<" outside "<<varrefp->varp()->msb()<<":0");
	    }
	}
	return false;  // Not a transform, so NOP
    }

    static bool operandsSame(AstNode* node1p, AstNode* node2p) {
	// For now we just detect constants & simple vars, though it could be more generic
	if (node1p->castConst() && node2p->castConst()) {
	    // Match ignoring any X values
	    V3Number num (node1p->fileline(), 1);
	    num.opCaseEq(node1p->castConst()->num(), node2p->castConst()->num());
	    return num.isNeqZero();
	}
	else if (node1p->castVarRef() && node2p->castVarRef()
		 && node1p->sameTree(node2p)) {
	    // Same variable
	    return true;
	} else {
	    return false;
	}
    }
    bool ifSameAssign(AstNodeIf* nodep) {
	AstNodeAssign* ifp = nodep->ifsp()->castNodeAssign();
	AstNodeAssign* elsep = nodep->elsesp()->castNodeAssign();
	if (!ifp   || ifp->nextp()) return false;  // Must be SINGLE statement
	if (!elsep || elsep->nextp()) return false;
	if (ifp->type() != elsep->type()) return false;  // Can't mix an assigndly and an assign
	AstVarRef* ifvarp = ifp->lhsp()->castVarRef();
	AstVarRef* elsevarp = elsep->lhsp()->castVarRef();
	if (!ifvarp || !elsevarp) return false;
	if (ifvarp->isWide()) return false;  // Would need temporaries, so not worth it
	if (ifvarp->varp() != elsevarp->varp()) return false;
	return true;
    }
    bool operandIfIf(AstNodeIf* nodep) {
	if (nodep->elsesp()) return false;
	AstNodeIf* lowerIfp = nodep->ifsp()->castNodeIf();
	if (!lowerIfp || lowerIfp->nextp()) return false;
	if (nodep->type() != lowerIfp->type()) return false;
	if (afterComment(lowerIfp->elsesp())) return false;
	return true;
    }

    //----------------------------------------
    // Constant Replacement functions.
    // These all take a node, delete its tree, and replaces it with a constant

    void replaceNum (AstNode* oldp, const V3Number& num) {
	// Replace oldp node with a constant set to specified value
	UASSERT (oldp, "Null old\n");
	if (oldp->castConst()) oldp->v3fatalSrc("Already constant??\n");
	AstNode* newp = new AstConst(oldp->fileline(), num);
	newp->widthSignedFrom(oldp);
	if (debug()>5) oldp->dumpTree(cout,"  const_old: ");
	if (debug()>5) newp->dumpTree(cout,"       _new: ");
	oldp->replaceWith(newp);
	oldp->deleteTree(); oldp=NULL;
    }
    void replaceNum (AstNode* nodep, uint32_t val) {
	V3Number num (nodep->fileline(), nodep->width(), val);
	replaceNum(nodep, num); nodep=NULL;
    }
    void replaceNumSigned(AstNodeBiop* nodep, uint32_t val) {
	// We allow both sides to be constant, as one may have come from parameter propagation, etc.
	if (m_warn && !(nodep->lhsp()->castConst() && nodep->rhsp()->castConst())) {
	    nodep->v3warn(UNSIGNED,"Comparison is constant due to unsigned arithmetic\n");
	}
	replaceNum(nodep, val); nodep=NULL;
    }
    void replaceNumLimited(AstNodeBiop* nodep, uint32_t val) {
	// Avoids gcc warning about same
	if (m_warn) nodep->v3warn(CMPCONST,"Comparison is constant due to limited range\n");
	replaceNum(nodep, val); nodep=NULL;
    }
    void replaceZero(AstNode* nodep) {
	replaceNum(nodep, 0); nodep=NULL;
    }
    void replaceConst(AstNodeUniop* nodep) {
	V3Number num (nodep->fileline(), nodep->width());
	nodep->numberOperate(num, nodep->lhsp()->castConst()->num());
	UINFO(4,"UNICONST -> "<<num<<endl);
	replaceNum(nodep, num); nodep=NULL;
    }
    void replaceConst(AstNodeBiop* nodep) {
	V3Number num (nodep->fileline(), nodep->width());
	nodep->numberOperate(num, nodep->lhsp()->castConst()->num(), nodep->rhsp()->castConst()->num());
	UINFO(4,"BICONST -> "<<num<<endl);
	replaceNum(nodep, num); nodep=NULL;
    }
    void replaceConst(AstNodeTriop* nodep) {
	V3Number num (nodep->fileline(), nodep->width());
	nodep->numberOperate(num, nodep->lhsp()->castConst()->num(),
			     nodep->rhsp()->castConst()->num(),
			     nodep->thsp()->castConst()->num());
	UINFO(4,"TRICONST -> "<<num<<endl);
	replaceNum(nodep, num); nodep=NULL;
    }

    //----------------------------------------
    // Replacement functions.
    // These all take a node and replace it with something else

    void replaceWChild(AstNode* nodep, AstNode* childp) {
	// NODE(..., CHILD(...)) -> CHILD(...)
	childp->unlinkFrBackWithNext();
	childp->widthSignedFrom(nodep);
	nodep->replaceWith(childp);
	nodep->deleteTree(); nodep=NULL;
    }
    void replaceWLhs(AstNodeUniop* nodep) {
	// Keep LHS, remove RHS
	replaceWChild(nodep, nodep->lhsp());
    }
    void replaceWLhs(AstNodeBiop* nodep) {
	// Keep LHS, remove RHS
	replaceWChild(nodep, nodep->lhsp());
    }
    void replaceWRhs(AstNodeBiop* nodep) {
	// Keep RHS, remove LHS
	replaceWChild(nodep, nodep->rhsp());
    }
    void replaceAsvConst (AstNodeBiop* nodep) {
	// BIASV(CONSTa, BIASV(CONSTb, c)) -> BIASV( BIASV_CONSTED(a,b), c)
	//nodep->dumpTree(cout, "  repAsvConst_old: ");
	AstNode* ap = nodep->lhsp();
	AstNodeBiop* rp = nodep->rhsp()->castNodeBiop();
	AstNode* bp = rp->lhsp();
	AstNode* cp = rp->rhsp();
	ap->unlinkFrBack();
	bp->unlinkFrBack();
	cp->unlinkFrBack();
	rp->unlinkFrBack();
	nodep->lhsp(rp);
	nodep->rhsp(cp);
	rp->lhsp(ap);
	rp->rhsp(bp);
	replaceConst(rp);
	//nodep->dumpTree(cout, "  repAsvConst_new: ");
    }
    void replaceExtend (AstNode* nodep, AstNode* arg0p) {
	// -> EXTEND(nodep)
	// like a AstExtend{$rhsp}, but we need to set the width correctly from base node
	arg0p->unlinkFrBack();
	AstNode* newp = (nodep->castExtendS()
			 ? (new AstExtendS(nodep->fileline(), arg0p))->castNode()
			 : (new AstExtend (nodep->fileline(), arg0p))->castNode());
	newp->widthSignedFrom(nodep);
	nodep->replaceWith(newp); nodep->deleteTree(); nodep=NULL;
    }
    void replacePowShift (AstNodeBiop* nodep) {  // Pow or PowS
	UINFO(5,"POW(2,b)->SHIFTL(1,b) "<<nodep<<endl);
	AstNode* rhsp = nodep->rhsp()->unlinkFrBack();
	AstShiftL* newp = new AstShiftL(nodep->fileline(),
					new AstConst(nodep->fileline(), 1),
					rhsp);
	newp->widthSignedFrom(nodep);
	newp->lhsp()->widthSignedFrom(nodep);
	nodep->replaceWith(newp); nodep->deleteTree(); nodep=NULL;
    }
    void replaceShiftOp (AstNodeBiop* nodep) {
	UINFO(5,"SHIFT(AND(a,b),CONST)->AND(SHIFT(a,CONST),SHIFT(b,CONST)) "<<nodep<<endl);
	AstNRelinker handle;
	nodep->unlinkFrBack(&handle);
	AstNodeBiop* lhsp = nodep->lhsp()->castNodeBiop(); lhsp->unlinkFrBack();
	AstNode* shiftp = nodep->rhsp()->unlinkFrBack();
	AstNode* ap = lhsp->lhsp()->unlinkFrBack();
	AstNode* bp = lhsp->rhsp()->unlinkFrBack();
	AstNodeBiop* shift1p = nodep;
	AstNodeBiop* shift2p = nodep->cloneTree(true)->castNodeBiop();
	shift1p->lhsp(ap); shift1p->rhsp(shiftp->cloneTree(true));
	shift2p->lhsp(bp); shift2p->rhsp(shiftp);
	AstNodeBiop* newp = lhsp;
	newp->lhsp(shift1p); newp->rhsp(shift2p);
	handle.relink(newp);
	newp->accept(*this);	// Further reduce, either node may have more reductions.
    }
    void replaceShiftShift (AstNodeBiop* nodep) {
	UINFO(4,"SHIFT(SHIFT(a,s1),s2)->SHIFT(a,ADD(s1,s2)) "<<nodep<<endl);
	if (debug()>=9) nodep->dumpTree(cout, "  repShiftShift_old: ");
	AstNodeBiop* lhsp = nodep->lhsp()->castNodeBiop(); lhsp->unlinkFrBack();
	AstNode* ap = lhsp->lhsp()->unlinkFrBack();
	AstNode* shift1p = lhsp->rhsp()->unlinkFrBack();
	AstNode* shift2p = nodep->rhsp()->unlinkFrBack();
	if (nodep->type()==lhsp->type()) {
	    nodep->lhsp(ap);
	    nodep->rhsp(new AstAdd(nodep->fileline(), shift1p, shift2p));
	    nodep->accept(*this);	// Further reduce, either node may have more reductions.
	} else {
	    // We know shift amounts are constant, but might be a mixed left/right shift
	    int shift1 = shift1p->castConst()->asInt(); if (lhsp->castShiftR())  shift1=-shift1;
	    int shift2 = shift2p->castConst()->asInt(); if (nodep->castShiftR()) shift2=-shift2;
	    int newshift = shift1+shift2;
	    AstNode* newp;
	    V3Number mask1 (nodep->fileline(), nodep->width());
	    V3Number ones (nodep->fileline(), nodep->width());
	    ones.opNot(mask1);
	    if (shift1<0) {
		mask1.opShiftR(ones,V3Number(nodep->fileline(),VL_WORDSIZE,-shift1));
	    } else {
		mask1.opShiftL(ones,V3Number(nodep->fileline(),VL_WORDSIZE,shift1));
	    }
	    V3Number mask (nodep->fileline(), nodep->width());
	    if (shift2<0) {
		mask.opShiftR(mask1,V3Number(nodep->fileline(),VL_WORDSIZE,-shift2));
	    } else {
		mask.opShiftL(mask1,V3Number(nodep->fileline(),VL_WORDSIZE,shift2));
	    }
	    if (newshift<0) {
		newp = new AstShiftR(nodep->fileline(), ap,
				     new AstConst(nodep->fileline(), -newshift));
	    } else {
		newp = new AstShiftL(nodep->fileline(), ap,
				     new AstConst(nodep->fileline(), newshift));
	    }
	    newp->widthSignedFrom(nodep);
	    newp = new AstAnd (nodep->fileline(),
			       newp,
			       new AstConst (nodep->fileline(), mask));
	    newp->widthSignedFrom(nodep);
	    nodep->replaceWith(newp); nodep->deleteTree(); nodep=NULL;
	    //newp->dumpTree(cout, "  repShiftShift_new: ");
	    newp->accept(*this);	// Further reduce, either node may have more reductions.
	}
	lhsp->deleteTree(); lhsp=NULL;
    }

    bool replaceAssignMultiSel(AstNodeAssign* nodep) {
	// Multiple assignments to sequential bits can be concated
	// ASSIGN(SEL(a),aq), ASSIGN(SEL(a+1),bq) -> ASSIGN(SEL(a:b),CONCAT(aq,bq)
	// ie. assign var[2]=a, assign var[3]=b -> assign var[3:2]={b,a}
	if (!m_modp) return false;   // Skip if we're not const'ing an entire module (IE doing only one assign, etc)
	AstSel* sel1p = nodep->lhsp()->castSel(); if (!sel1p) return false;
	AstNodeAssign* nextp = nodep->nextp()->castNodeAssign(); if (!nextp) return false;
	if (nodep->type() != nextp->type()) return false;
	AstSel* sel2p = nextp->lhsp()->castSel(); if (!sel2p) return false;
	AstVarRef* varref1p = sel1p->fromp()->castVarRef(); if (!varref1p) return false;
	AstVarRef* varref2p = sel2p->fromp()->castVarRef(); if (!varref2p) return false;
	if (varref1p->varp() != varref2p->varp()) return false;
	AstConst* con1p = sel1p->lsbp()->castConst(); if (!con1p) return false;
	AstConst* con2p = sel2p->lsbp()->castConst(); if (!con2p) return false;
	// We need to make sure there's no self-references involved in either
	// assignment.  For speed, we only look 3 deep, then give up.
	if (!varNotReferenced(nodep->rhsp(), varref1p->varp())) return false;
	if (!varNotReferenced(nextp->rhsp(), varref2p->varp())) return false;
	// Swap?
	if ((  con1p->asInt() != con2p->asInt() + sel2p->width())
	    &&(con2p->asInt() != con1p->asInt() + sel1p->width())) return false;
	bool lsbFirstAssign = (con1p->asInt() < con2p->asInt());
	// If the user already has nice 32-bit divisions, keep them to aid later subdivision
	//if (VL_BITBIT_I(con1p->asInt()) == 0) return false;
	UINFO(4,"replaceAssignMultiSel "<<nodep<<endl);
	UINFO(4,"                   && "<<nextp<<endl);
	//nodep->dumpTree(cout, "comb1: ");
	//nextp->dumpTree(cout, "comb2: ");
	AstNode* rhs1p = nodep->rhsp()->unlinkFrBack();
	AstNode* rhs2p = nextp->rhsp()->unlinkFrBack();
	AstNode* newp;
	if (lsbFirstAssign) {
	    newp = nodep->cloneType (new AstSel(sel1p->fileline(), varref1p->unlinkFrBack(),
						sel1p->lsbConst(), sel1p->width() + sel2p->width()),
				     new AstConcat(rhs1p->fileline(), rhs2p, rhs1p));
	} else {
	    newp = nodep->cloneType (new AstSel(sel1p->fileline(), varref1p->unlinkFrBack(),
						sel2p->lsbConst(), sel1p->width() + sel2p->width()),
				     new AstConcat(rhs1p->fileline(), rhs1p, rhs2p));
	}
	//pnewp->dumpTree(cout, "conew: ");
	nodep->replaceWith(newp); nodep->deleteTree();
	nextp->unlinkFrBack()->deleteTree();
	return true;
    }

    bool varNotReferenced(AstNode* nodep, AstVar* varp, bool level=0) {
	// Return true if varp never referenced under node.
	// Return false if referenced, or tree too deep to be worth it
	if (!nodep) return true;
	if (level>2) return false;
	if (nodep->castNodeVarRef() && nodep->castNodeVarRef()->varp()==varp) return false;
	return (varNotReferenced  (nodep->nextp(),varp,level+1)
		&& varNotReferenced(nodep->op1p(),varp,level+1)
		&& varNotReferenced(nodep->op2p(),varp,level+1)
		&& varNotReferenced(nodep->op3p(),varp,level+1)
		&& varNotReferenced(nodep->op4p(),varp,level+1));
    }

    bool replaceNodeAssign(AstNodeAssign* nodep) {
	if (nodep->lhsp()->castVarRef()
	    && nodep->rhsp()->castVarRef()
	    && nodep->lhsp()->sameTree(nodep->rhsp())
	    && !nodep->castAssignDly()) {
	    // X = X.  Quite pointless, though X <= X may override another earlier assignment
	    if (nodep->castAssignW()) {
		nodep->v3error("Wire inputs its own output, creating circular logic (wire x=x)");
		return false;  // Don't delete the assign, or V3Gate will freak out
	    } else {
		nodep->unlinkFrBack()->deleteTree(); nodep=NULL;
		return true;
	    }
	}
	else if (!m_cpp && nodep->lhsp()->castConcat()) {
	    UINFO(4,"  ASSI "<<nodep<<endl);
	    if (debug()>=9) nodep->dumpTree(cout,"  Ass_old: ");
	    // ASSIGN(CONCAT(lc1,lc2),rhs) -> ASSIGN(lc1,SEL(rhs,{size})),
	    //				      ASSIGN(lc2,SEL(newrhs,{size}))
	    // Unlink the stuff
	    AstNode*   lc1p    = nodep->lhsp()->castConcat()->lhsp()->unlinkFrBack();
	    AstNode*   lc2p    = nodep->lhsp()->castConcat()->rhsp()->unlinkFrBack();
	    AstNode*   conp    = nodep->lhsp()->castConcat()->unlinkFrBack();
	    AstNode*   rhsp    = nodep->rhsp()->unlinkFrBack();
	    AstNode*   rhs2p   = rhsp->cloneTree(false);
	    // Calc widths
	    int lsb2 = 0;
	    int msb2 = lsb2+lc2p->width()-1;
	    int lsb1 = msb2+1;
	    int msb1 = lsb1+lc1p->width()-1;
	    if (msb1!=(conp->width()-1)) nodep->v3fatalSrc("Width calc mismatch");
	    // Form ranges
	    AstSel*  sel1p = new AstSel(conp->fileline(), rhsp,  lsb1, msb1-lsb1+1);
	    AstSel*  sel2p = new AstSel(conp->fileline(), rhs2p, lsb2, msb2-lsb2+1);
	    sel1p->width(msb1-lsb1+1,msb1-lsb1+1);
	    sel2p->width(msb2-lsb2+1,msb2-lsb2+1);
	    // Make new assigns of same flavor as old one
	    //*** Not cloneTree; just one node.
	    AstNodeAssign* asn1p=nodep->cloneType(lc1p, sel1p)->castNodeAssign();
	    AstNodeAssign* asn2p=nodep->cloneType(lc2p, sel2p)->castNodeAssign();
	    asn1p->width(msb1-lsb1+1,msb1-lsb1+1);
	    asn2p->width(msb2-lsb2+1,msb2-lsb2+1);
	    nodep->addNextHere(asn1p);
	    nodep->addNextHere(asn2p);
	    if (debug()>=9) asn1p->dumpTree(cout,"     _new: ");
	    if (debug()>=9) asn2p->dumpTree(cout,"     _new: ");
	    // Cleanup
	    nodep->unlinkFrBack()->deleteTree();
	    // Further reduce, either node may have more reductions.
	    return true;
	}
	else if (replaceAssignMultiSel(nodep)) {
	    return true;
	}
	return false;
    }

    // Boolean replacements
    bool operandBoolShift(AstNode* nodep) {
	// boolean test of AND(const,SHIFTR(x,const)) -> test of AND(SHIFTL(x,const), x)
	if (!nodep->castAnd()) return false;
	if (!nodep->castAnd()->lhsp()->castConst()) return false;
	if (!nodep->castAnd()->rhsp()->castShiftR()) return false;
	AstShiftR* shiftp = nodep->castAnd()->rhsp()->castShiftR();
	if (!shiftp->rhsp()->castConst()) return false;
	if ((uint32_t)(nodep->width()) <= shiftp->rhsp()->castConst()->asInt()) return false;
	return true;
    }
    void replaceBoolShift(AstNode* nodep) {
	if (debug()>=9) nodep->dumpTree(cout,"  bshft_old: ");
	AstConst* andConstp = nodep->castAnd()->lhsp()->castConst();
	AstNode* fromp = nodep->castAnd()->rhsp()->castShiftR()->lhsp()->unlinkFrBack();
	AstConst* shiftConstp = nodep->castAnd()->rhsp()->castShiftR()->rhsp()->castConst();
	V3Number val (andConstp->fileline(), andConstp->width());
	val.opShiftL(andConstp->num(), shiftConstp->num());
	AstAnd* newp = new AstAnd(nodep->fileline(),
				  new AstConst(nodep->fileline(), val),
				  fromp);
	newp->width(nodep->width(), nodep->width());  // widthMin no longer applicable
	nodep->replaceWith(newp);
	nodep->deleteTree(); nodep=NULL;
	if (debug()>=9) newp->dumpTree(cout,"       _new: ");
    }

    //----------------------------------------

    // VISITORS
    virtual void visit(AstNetlist* nodep, AstNUser*) {
	// Iterate modules backwards, in bottom-up order.  That's faster
	nodep->iterateChildrenBackwards(*this);
    }
    virtual void visit(AstModule* nodep, AstNUser*) {
	m_modp = nodep;
	nodep->iterateChildren(*this);
	m_modp = NULL;
    }
    virtual void visit(AstCFunc* nodep, AstNUser*) {
	// No ASSIGNW removals under funcs, we've long eliminated INITIALs
	// (We should perhaps rename the assignw's to just assigns)
	m_wremove = false;
	nodep->iterateChildren(*this);
	m_wremove = true;
    }
    virtual void visit(AstScope* nodep, AstNUser*) {
	// No ASSIGNW removals under scope, we've long eliminated INITIALs
	m_scopep = nodep;
	m_wremove = false;
	nodep->iterateChildren(*this);
	m_wremove = true;
	m_scopep = NULL;
    }

    void swapSides(AstNodeBiCom* nodep) {
	// COMMUNATIVE({a},CONST) -> COMMUNATIVE(CONST,{a})
	// This simplifies later optimizations
	AstNode* lhsp = nodep->lhsp()->unlinkFrBackWithNext();
	AstNode* rhsp = nodep->rhsp()->unlinkFrBackWithNext();
	nodep->lhsp(rhsp);
	nodep->rhsp(lhsp);
	nodep->accept(*this);  // Again?
    }

    int operandConcatMove(AstConcat* nodep) {
	//    CONCAT under concat  (See moveConcat)
	// Return value: true indicates to do it; 2 means move to LHS
	AstConcat* abConcp = nodep->lhsp()->castConcat();
	AstConcat* bcConcp = nodep->rhsp()->castConcat();
	if (!abConcp && !bcConcp) return 0;
	if (bcConcp) {
	    AstNode* ap = nodep->lhsp();
	    AstNode* bp = bcConcp->lhsp();
	    // If a+b == 32,64,96 etc, then we want to have a+b together on LHS
	    if (VL_BITBIT_I(ap->width()+bp->width())==0) return 2;  // Transform 2: to abConc
	}
	else { //abConcp
	    // Unless lhs is already 32 bits due to above, reorder it
	    if (VL_BITBIT_I(nodep->lhsp()->width())!=0) return 1;  // Transform 1: to bcConc
	}
	return 0;  // ok
    }
    void moveConcat(AstConcat* nodep) {
	//    1: CONCAT(CONCAT({a},{b}),{c})  -> CONCAT({a},CONCAT({b}, {c}))
	// or 2: CONCAT({a}, CONCAT({b},{c})) -> CONCAT(CONCAT({a},{b}),{c})
	// Because the lhs of a concat needs a shift but the rhs doesn't,
	// putting additional CONCATs on the RHS leads to fewer assembler operations.
	// However, we'll end up with lots of wide moves if we make huge trees
	// like that, so on 32 bit boundaries, we'll do the opposite form.
	UINFO(4,"Move concat: "<<nodep<<endl);
	if (operandConcatMove(nodep)>1) {
	    AstNode* ap = nodep->lhsp()->unlinkFrBack();
	    AstConcat* bcConcp = nodep->rhsp()->castConcat(); bcConcp->unlinkFrBack();
	    AstNode* bp = bcConcp->lhsp()->unlinkFrBack();
	    AstNode* cp = bcConcp->rhsp()->unlinkFrBack();
	    AstConcat* abConcp = new AstConcat(bcConcp->fileline(),
					       ap, bp);
	    nodep->lhsp(abConcp);
	    nodep->rhsp(cp);
	    // If bp was a concat, then we have this exact same form again!
	    // Recurse rather then calling node->iterate to prevent 2^n recursion!
	    if (operandConcatMove(abConcp)) moveConcat(abConcp);
	} else { 
	    AstConcat* abConcp = nodep->lhsp()->castConcat(); abConcp->unlinkFrBack();
	    AstNode* ap = abConcp->lhsp()->unlinkFrBack();
	    AstNode* bp = abConcp->rhsp()->unlinkFrBack();
	    AstNode* cp = nodep->rhsp()->unlinkFrBack();
	    AstConcat* bcConcp = new AstConcat(abConcp->fileline(),
					       bp, cp);
	    nodep->lhsp(ap);
	    nodep->rhsp(bcConcp);
	    if (operandConcatMove(bcConcp)) moveConcat(bcConcp);
	}
    }

    // Special cases
    virtual void visit(AstConst* nodep, AstNUser*) {}	// Already constant

    virtual void visit(AstCell* nodep, AstNUser*) {
	if (m_params) {
	    nodep->paramsp()->iterateAndNext(*this);
	} else {
	    nodep->iterateChildren(*this);
	}
    }
    virtual void visit(AstPin* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
    }

    void replaceSelSel(AstSel* nodep) {
	// SEL(SEL({x},a,b),c,d) => SEL({x},a+c,d)
	AstSel* belowp = nodep->fromp()->castSel();
	AstNode* fromp = belowp->fromp()->unlinkFrBack();
	AstNode* widthp = nodep->widthp()->unlinkFrBack();
	AstNode* lsb1p = nodep->lsbp()->unlinkFrBack();
	AstNode* lsb2p = belowp->lsbp()->unlinkFrBack();
	// Eliminate lower range
	UINFO(4,"Elim Lower range: "<<nodep<<endl);
	AstNode* newlsbp;
	if (lsb1p->castConst() && lsb2p->castConst()) {
	    newlsbp = new AstConst(lsb1p->fileline(),
				   lsb1p->castConst()->asInt() + lsb2p->castConst()->asInt());
	} else {
	    // Width is important, we need the width of the fromp's
	    // expression, not the potentially smaller lsb1p's width
	    newlsbp = new AstAdd(lsb1p->fileline(),
				 lsb2p, new AstExtend(lsb1p->fileline(), lsb1p));
	    newlsbp->widthFrom(lsb2p); // Unsigned
	    newlsbp->castAdd()->rhsp()->widthFrom(lsb2p); // Unsigned
	}
	AstSel* newp = new AstSel(nodep->fileline(),
				  fromp,
				  newlsbp,
				  widthp);
	nodep->replaceWith(newp);
	nodep->deleteTree(); nodep=NULL;
    }

    void replaceSelConcat(AstSel* nodep) {
	// SEL(CONCAT(a,b),c,d) => SEL(a or b, . .)
	AstConcat* conp = nodep->fromp()->castConcat();
	AstNode* conLhsp = conp->lhsp();
	AstNode* conRhsp = conp->rhsp();
	if ((int)nodep->lsbConst() >= conRhsp->width()) {
	    conLhsp->unlinkFrBack();
	    AstSel* newp = new AstSel(nodep->fileline(),
				      conLhsp,
				      nodep->lsbConst() - conRhsp->width(),
				      nodep->widthConst());
	    nodep->replaceWith(newp);
	}
	else if ((int)nodep->msbConst() < conRhsp->width()) {
	    conRhsp->unlinkFrBack();
	    AstSel* newp = new AstSel(nodep->fileline(),
				      conRhsp,
				      nodep->lsbConst(),
				      nodep->widthConst());
	    nodep->replaceWith(newp);
	}
	else {
	    // Yuk, split between the two
	    conRhsp->unlinkFrBack();
	    conLhsp->unlinkFrBack();
	    AstConcat* newp = new AstConcat
		(nodep->fileline(),
		 new AstSel(nodep->fileline(),
			    conLhsp,
			    0,
			    nodep->msbConst() - conRhsp->width() + 1),
		 new AstSel(nodep->fileline(),
			    conRhsp,
			    nodep->lsbConst(),
			    conRhsp->width()-nodep->lsbConst()));
	    nodep->replaceWith(newp);
	}
	nodep->deleteTree(); nodep=NULL;
    }

    void replaceSelReplicate(AstSel* nodep) {
	// SEL(REPLICATE(a,b),1,bit) => SEL(a,1,bit)
	AstReplicate* repp = nodep->fromp()->castReplicate();
	AstNode* fromp = repp->lhsp()->unlinkFrBack();
	AstConst* lsbp = nodep->lsbp()->castConst();
	AstNode* widthp = nodep->widthp()->unlinkFrBack();
	AstSel* newp = new AstSel(nodep->fileline(),
				  fromp,
				  new AstConst(lsbp->fileline(), lsbp->asInt() % fromp->width()),
				  widthp);
	newp->widthSignedFrom(nodep);
	nodep->replaceWith(newp); nodep->deleteTree(); nodep=NULL;
    }

    virtual void visit(AstVarRef* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	if (!nodep->varp()) nodep->v3fatalSrc("Not linked");
	bool did=false;
	if (!m_cpp && nodep->varp()->hasSimpleInit()) {
	    //if (debug()) nodep->varp()->initp()->dumpTree(cout,"  visitvaref: ");
	    nodep->varp()->initp()->iterateAndNext(*this);
	    if (operandConst(nodep->varp()->initp())
		&& !nodep->lvalue()
		&& ((v3Global.opt.oConst() && !m_params) // Can reduce constant wires into equations
		    || nodep->varp()->isParam())) {
		AstConst* constp = nodep->varp()->initp()->castConst();
		const V3Number& num = constp->num();
		//UINFO(2,"constVisit "<<(void*)constp<<" "<<num<<endl);
		replaceNum(nodep, num); nodep=NULL;
		did=true;
	    }
	}
	if (!did && m_required) {
	    nodep->v3error("Expecting expression to be constant, but variable isn't const: "<<nodep->varp()->prettyName());
	}
    }
    virtual void visit(AstAttrOf* nodep, AstNUser*) {
	// Don't iterate children, don't want to loose VarRef.
	if (nodep->attrType()==AstAttrType::SCOPE_TEXT) {
	} else if (nodep->attrType()==AstAttrType::BITS) {
	    if (!nodep->fromp() || !nodep->fromp()->widthMin()) nodep->v3fatalSrc("Unsized expression");
	    V3Number num (nodep->fileline(), 32, nodep->fromp()->widthMin());
	    replaceNum(nodep, num); nodep=NULL;
	} else {
	    if (!nodep->fromp()->castNodeVarRef()) nodep->v3fatalSrc("Not linked");
	    AstVar* varp = nodep->fromp()->castNodeVarRef()->varp();
	    if (!varp) nodep->v3fatalSrc("Not linked");
	    if (nodep->attrType()==AstAttrType::RANGE_LSB) {
		if (!varp->rangep()) nodep->v3fatalSrc("RANGE_LSB on vec w/o range\n");
		if (operandConst(varp->rangep()->lsbp())) {
		    V3Number num (nodep->fileline(), 32, varp->lsb());
		    replaceNum(nodep, num); nodep=NULL;
		}
	    } else if (nodep->attrType()==AstAttrType::ARRAY_LSB) {
		AstRange* arrayp=varp->arrayp(nodep->dimension());
		if (!arrayp) nodep->v3fatalSrc("ARRAY_LSB on vec w/o range or right # dimensions\n");
		if (operandConst(arrayp->lsbp())) {
		    V3Number num (nodep->fileline(), 32, arrayp->lsbConst());
		    replaceNum(nodep, num); nodep=NULL;
		}
	    } else nodep->v3fatalSrc("Missing ATTR type case\n");
	}
    }
    virtual void visit(AstSenItem* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	if (nodep->sensp()->castConst()
	    || (nodep->varrefp() && nodep->varrefp()->varp()->isParam())) {
	    // Constants in sensitivity lists may be removed (we'll simplify later)
	    AstSenItem* newp = new AstSenItem(nodep->fileline(), AstSenItem::Never());
	    nodep->replaceWith(newp);
	    nodep->deleteTree(); nodep=NULL;
	} else if (nodep->sensp()->castNot()) {
	    // V3Gate may propagate NOTs into clocks... Just deal with it
	    AstNode* sensp = nodep->sensp();
	    AstNode* lastSensp = sensp;
	    bool invert = false;
	    while (lastSensp->castNot()) {
		lastSensp = lastSensp->castNot()->lhsp();
		invert = !invert;
	    }
	    UINFO(8,"senItem(NOT...) "<<nodep<<" "<<invert<<endl);
	    if (invert) nodep->edgeType( nodep->edgeType().invert() );
	    AstVarRef* senvarp = lastSensp->unlinkFrBack()->castVarRef();
	    if (!senvarp) sensp->v3fatalSrc("Non-varref sensitivity variable");
	    sensp->replaceWith(senvarp);
	    sensp->deleteTree(); sensp=NULL;
	} else {
	    if (nodep->hasVar() && !nodep->varrefp()) nodep->v3fatalSrc("Null sensitivity variable");
	}
    }

    //-----
    // Zero elimination
    virtual void visit(AstNodeAssign* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	replaceNodeAssign(nodep);
    }
    virtual void visit(AstAssignAlias* nodep, AstNUser*) {
	// Don't perform any optimizations, keep the alias around
    }
    virtual void visit(AstAssignW* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	if (replaceNodeAssign(nodep)) return;
	AstNodeVarRef* varrefp = nodep->lhsp()->castVarRef();  // Not VarXRef, as different refs may set different values to each hierarchy
	if (m_wremove
	    && m_modp && operandConst(nodep->rhsp())
	    && !nodep->rhsp()->castConst()->num().isFourState()
	    && varrefp		// Don't do messes with BITREFs/ARRAYREFs
	    && !varrefp->varp()->initp()  // Not already constified
	    && !varrefp->varScopep()	// Not scoped (or each scope may have different initial value)
	    && !m_params) {
	    // ASSIGNW (VARREF, const) -> INITIAL ( ASSIGN (VARREF, const) )
	    UINFO(4,"constAssignW "<<nodep<<endl);
	    // Make a initial assignment
	    AstNode* exprp = nodep->rhsp()->unlinkFrBack();
	    varrefp->unlinkFrBack();
	    AstInitial* newinitp
		= new AstInitial(nodep->fileline(),
				 new AstAssign(nodep->fileline(),
					       varrefp, exprp));
	    m_modp->addStmtp(newinitp);
	    nodep->unlinkFrBack();  nodep=NULL;
	    // Set the initial value right in the variable so we can constant propagate
	    AstNode* initvaluep = exprp->cloneTree(false);
	    varrefp->varp()->initp(initvaluep);
	}
    }

    virtual void visit(AstNodeIf* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	if (AstConst* constp = nodep->condp()->castConst()) {
	    AstNode* keepp = NULL;
	    if (constp->isZero()) {
		UINFO(4,"IF(0,{any},{x}) => {x}: "<<nodep<<endl);
		keepp = nodep->elsesp();
	    } else {
		UINFO(4,"IF(!0,{x},{any}) => {x}: "<<nodep<<endl);
		keepp = nodep->ifsp();
	    }
	    if (keepp) {
		keepp->unlinkFrBackWithNext();
		nodep->replaceWith(keepp);
	    } else {
		nodep->unlinkFrBack();
	    }
	    nodep->deleteTree(); nodep=NULL;
	}
	else if (!afterComment(nodep->ifsp()) && !afterComment(nodep->elsesp())) {
	    // Empty block, remove it
	    // Note if we support more C++ then there might be side effects in the condition itself
	    nodep->unlinkFrBack()->deleteTree(); nodep=NULL;
	}
	else if (!afterComment(nodep->ifsp())) {
	    UINFO(4,"IF({x}) NULL {...} => IF(NOT{x}}: "<<nodep<<endl);
	    AstNode* condp = nodep->condp();
	    AstNode* elsesp = nodep->elsesp();
	    condp->unlinkFrBackWithNext();
	    elsesp->unlinkFrBackWithNext();
	    if (nodep->ifsp()) { nodep->ifsp()->unlinkFrBackWithNext()->deleteTree(); }  // Must have been comment
	    nodep->condp(new AstLogNot(condp->fileline(), condp));  // LogNot, as C++ optimization also possible
	    nodep->addIfsp(elsesp);
	}
	else if (((nodep->condp()->castNot() && nodep->condp()->width()==1)
		  || (nodep->condp()->castLogNot()))
		 && nodep->ifsp() && nodep->elsesp()) {
	    UINFO(4,"IF(NOT {x})  => IF(x) swapped if/else"<<nodep<<endl);
	    AstNode* condp = nodep->condp()->castNot()->lhsp()->unlinkFrBackWithNext();
	    AstNode* ifsp = nodep->ifsp()->unlinkFrBackWithNext();	
	    AstNode* elsesp = nodep->elsesp()->unlinkFrBackWithNext();
	    AstIf* ifp = new AstIf(nodep->fileline(), condp, elsesp, ifsp);
	    ifp->branchPred(nodep->branchPred().invert());
	    nodep->replaceWith(ifp);
	    nodep->deleteTree(); nodep=NULL;
	}
	else if (ifSameAssign(nodep)) {
	    UINFO(4,"IF({a}) ASSIGN({b},{c}) else ASSIGN({b},{d}) => ASSIGN({b}, {a}?{c}:{d})"<<endl);
	    AstNodeAssign* ifp   = nodep->ifsp()->castNodeAssign();
	    AstNodeAssign* elsep = nodep->elsesp()->castNodeAssign();
	    ifp->unlinkFrBack();
	    AstNode* condp = nodep->condp()->unlinkFrBack();
	    AstNode* truep = ifp->rhsp()->unlinkFrBack();
	    AstNode* falsep = elsep->rhsp()->unlinkFrBack();
	    ifp->rhsp(new AstCond(truep->fileline(),
				  condp, truep, falsep));
	    nodep->replaceWith(ifp);
	    nodep->deleteTree(); nodep=NULL;
	}
	else if (0	// Disabled, as vpm assertions are faster without due to short-circuiting
		 && operandIfIf(nodep)) {  
	    UINFO(0,"IF({a}) IF({b}) => IF({a} && {b})"<<endl);
	    AstNodeIf* lowerIfp = nodep->ifsp()->castNodeIf();
	    AstNode* condp = nodep->condp()->unlinkFrBack();
	    AstNode* lowerIfsp = lowerIfp->ifsp()->unlinkFrBackWithNext();
	    AstNode* lowerCondp = lowerIfp->condp()->unlinkFrBackWithNext();
	    nodep->condp(new AstLogAnd(lowerIfp->fileline(),
				       condp, lowerCondp));
	    lowerIfp->replaceWith(lowerIfsp);
	    lowerIfp->deleteTree(); lowerIfp=NULL;
	}
	else if (operandBoolShift(nodep->condp())) {
	    replaceBoolShift(nodep->condp());
	}
    }

    virtual void visit(AstWhile* nodep, AstNUser*) {
	nodep->iterateChildren(*this);
	if (nodep->condp()->isZero()) {
	    UINFO(4,"WHILE(0) => nop "<<nodep<<endl);
	    nodep->unlinkFrBack();
	    nodep->deleteTree(); nodep=NULL;
	}
	else if (operandBoolShift(nodep->condp())) {
	    replaceBoolShift(nodep->condp());
	}
    }

    //-----
    // Below lines are magic expressions processed by astgen
    //  "AstNODETYPE {             # bracket not paren
    //                $accessor_name, ...
    //                             # ,, gets replaced with a , rather then &&
    //	             }" 	   # bracket not paren
    //    ,"function to call"
    // or ,"AstREPLACEMENT_TYPE{ $accessor }"

    // Lint Checks
    //    v--- *1* These ops are always first, as we warn before replacing
    //    v--- *V* This op is a verilog op, ignore when in m_cpp mode
    TREEOP1("AstSel{warnSelect(nodep)}",	"NEVER");
    // Generic constants on both side.  Do this first to avoid other replacements
    TREEOP("AstNodeBiop {$lhsp.castConst, $rhsp.castConst}",  "replaceConst(nodep)");
    TREEOP("AstNodeUniop{$lhsp.castConst}",	"replaceConst(nodep)");
    // Zero on one side or the other
    TREEOP("AstAdd   {$lhsp.isZero, $rhsp}",	"replaceWRhs(nodep)");
    TREEOP("AstAnd   {$lhsp.isZero, $rhsp}",	"replaceZero(nodep)");
    TREEOP("AstLogAnd{$lhsp.isZero, $rhsp}",	"replaceZero(nodep)");
    TREEOP("AstLogOr {$lhsp.isZero, $rhsp}",	"replaceWRhs(nodep)");
    TREEOP("AstDiv   {$lhsp.isZero, $rhsp}",	"replaceZero(nodep)");
    TREEOP("AstDivS  {$lhsp.isZero, $rhsp}",	"replaceZero(nodep)");
    TREEOP("AstMul   {$lhsp.isZero, $rhsp}",	"replaceZero(nodep)");
    TREEOP("AstMulS  {$lhsp.isZero, $rhsp}",	"replaceZero(nodep)");
    TREEOP("AstPow   {$lhsp.isZero, $rhsp}",	"replaceZero(nodep)");
    TREEOP("AstPowS  {$lhsp.isZero, $rhsp}",	"replaceZero(nodep)");
    TREEOP("AstOr    {$lhsp.isZero, $rhsp}",	"replaceWRhs(nodep)");
    TREEOP("AstShiftL{$lhsp.isZero, $rhsp}",	"replaceZero(nodep)");
    TREEOP("AstShiftR{$lhsp.isZero, $rhsp}",	"replaceZero(nodep)");
    TREEOP("AstShiftRS{$lhsp.isZero, $rhsp}",	"replaceZero(nodep)");
    TREEOP("AstXor   {$lhsp.isZero, $rhsp}",	"replaceWRhs(nodep)");
    TREEOP("AstSub   {$lhsp.isZero, $rhsp}",	"AstUnaryMin{$rhsp}");
    TREEOP("AstAdd   {$lhsp, $rhsp.isZero}",	"replaceWLhs(nodep)");
    TREEOP("AstAnd   {$lhsp, $rhsp.isZero}",	"replaceZero(nodep)");
    TREEOP("AstLogAnd{$lhsp, $rhsp.isZero}",	"replaceZero(nodep)");
    TREEOP("AstLogOr {$lhsp, $rhsp.isZero}",	"replaceWLhs(nodep)");
    TREEOP("AstMul   {$lhsp, $rhsp.isZero}",	"replaceZero(nodep)");
    TREEOP("AstMulS  {$lhsp, $rhsp.isZero}",	"replaceZero(nodep)");
    TREEOP("AstOr    {$lhsp, $rhsp.isZero}",	"replaceWLhs(nodep)");
    TREEOP("AstShiftL{$lhsp, $rhsp.isZero}",	"replaceWLhs(nodep)");
    TREEOP("AstShiftR{$lhsp, $rhsp.isZero}",	"replaceWLhs(nodep)");
    TREEOP("AstShiftRS{$lhsp, $rhsp.isZero}",	"replaceWLhs(nodep)");
    TREEOP("AstSub   {$lhsp, $rhsp.isZero}",	"replaceWLhs(nodep)");
    TREEOP("AstXor   {$lhsp, $rhsp.isZero}",	"replaceWLhs(nodep)");
    // Non-zero on one side or the other
    TREEOP("AstAnd   {$lhsp.isAllOnes, $rhsp}",	"replaceWRhs(nodep)");
    TREEOP("AstLogAnd{$lhsp.isNeqZero, $rhsp}",	"replaceWRhs(nodep)");
    TREEOP("AstOr    {$lhsp.isAllOnes, $rhsp}",	"replaceWLhs(nodep)"); //->allOnes
    TREEOP("AstLogOr {$lhsp.isNeqZero, $rhsp}",	"replaceNum(nodep,1)");
    TREEOP("AstAnd   {$lhsp, $rhsp.isAllOnes}",	"replaceWLhs(nodep)");
    TREEOP("AstLogAnd{$lhsp, $rhsp.isNeqZero}",	"replaceWLhs(nodep)");
    TREEOP("AstOr    {$lhsp, $rhsp.isAllOnes}",	"replaceWRhs(nodep)"); //->allOnes
    TREEOP("AstLogOr {$lhsp, $rhsp.isNeqZero}",	"replaceNum(nodep,1)");
    TREEOP("AstXor   {$lhsp.isAllOnes, $rhsp}",	"AstNot{$rhsp}");
    TREEOP("AstPow   {operandIsTwo($lhsp), $rhsp}","replacePowShift(nodep)");  // 2**a == 1<<a
    TREEOP("AstPowS  {operandIsTwo($lhsp), $rhsp}","replacePowShift(nodep)");  // 2**a == 1<<a
    // Trinary ops
    // Note V3Case::Sel requires Cond to always be conditionally executed in C to prevent core dump!
    TREEOP("AstNodeCond{$condp.isZero,       $expr1p, $expr2p}", "replaceWChild(nodep,$expr2p)");
    TREEOP("AstNodeCond{$condp.isNeqZero,    $expr1p, $expr2p}", "replaceWChild(nodep,$expr1p)");
    TREEOP("AstNodeCond{$condp, operandsSame($expr1p,,$expr2p)}","replaceWChild(nodep,$expr1p)");
    TREEOP("AstCond{$condp->castNot(),       $expr1p, $expr2p}", "AstCond{$condp->op1p(), $expr2p, $expr1p}");
    TREEOP("AstNodeCond{$condp.width1, $expr1p.width1,   $expr1p.isAllOnes, $expr2p}", "AstOr {$condp, $expr2p}");  // a?1:b == a|b
    TREEOP("AstNodeCond{$condp.width1, $expr1p.width1,   $expr1p,    $expr2p.isZero}", "AstAnd{$condp, $expr1p}");  // a?b:0 == a&b
    TREEOP("AstNodeCond{$condp.width1, $expr1p.width1,   $expr1p, $expr2p.isAllOnes}", "AstOr {AstNot{$condp}, $expr1p}");  // a?b:1 == ~a|b
    TREEOP("AstNodeCond{$condp.width1, $expr1p.width1,   $expr1p.isZero,    $expr2p}", "AstAnd{AstNot{$condp}, $expr2p}");  // a?0:b == ~a&b
    TREEOP("AstNodeCond{!$condp.width1, operandBoolShift(nodep->condp())}", "replaceBoolShift(nodep->condp())");
    // Prefer constants on left, since that often needs a shift, it lets constant red remove the shift
    TREEOP("AstNodeBiCom{$lhsp, $rhsp.castConst}",	"swapSides(nodep)");
    TREEOP("AstNodeBiComAsv{operandAsvConst(nodep)}",	"replaceAsvConst(nodep)");
    //    v--- *1* as These ops are always first, as we warn before replacing
    TREEOP1("AstLt   {$lhsp, $rhsp.isZero}",		"replaceNumSigned(nodep,0)");
    TREEOP1("AstGte  {$lhsp, $rhsp.isZero}",		"replaceNumSigned(nodep,1)");
    TREEOP1("AstGt   {$lhsp.isZero, $rhsp}",		"replaceNumSigned(nodep,0)");
    TREEOP1("AstLte  {$lhsp.isZero, $rhsp}",		"replaceNumSigned(nodep,1)");
    TREEOP1("AstGt   {$lhsp, $rhsp.isAllOnes, $lhsp->width()==$rhsp->width()}",  "replaceNumLimited(nodep,0)");
    TREEOP1("AstLte  {$lhsp, $rhsp.isAllOnes, $lhsp->width()==$rhsp->width()}",  "replaceNumLimited(nodep,1)");
    TREEOP1("AstLt   {$lhsp.isAllOnes, $rhsp, $lhsp->width()==$rhsp->width()}",  "replaceNumLimited(nodep,0)");
    TREEOP1("AstGte  {$lhsp.isAllOnes, $rhsp, $lhsp->width()==$rhsp->width()}",  "replaceNumLimited(nodep,1)");
    // Two level bubble pushing
    TREEOP ("AstNot   {$lhsp.castNot,  $lhsp->width()==$lhsp->castNot()->lhsp()->width()}",	"replaceWChild(nodep, $lhsp->op1p())");  // NOT(NOT(x))->x
    TREEOP ("AstLogNot{$lhsp.castLogNot}",		"replaceWChild(nodep, $lhsp->op1p())");  // NOT(NOT(x))->x
    TREEOPV("AstNot   {$lhsp.castEqCase, $lhsp.width1}","AstNeqCase{$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castEqCase}",		"AstNeqCase{$lhsp->op1p(),$lhsp->op2p()}");
    TREEOPV("AstNot   {$lhsp.castNeqCase, $lhsp.width1}","AstEqCase {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castNeqCase}",		"AstEqCase {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOPV("AstNot   {$lhsp.castEq, $lhsp.width1}",	"AstNeq {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castEq}",			"AstNeq {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOPV("AstNot   {$lhsp.castNeq, $lhsp.width1}",	"AstEq  {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castNeq}",			"AstEq  {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOPV("AstNot   {$lhsp.castLt, $lhsp.width1}",	"AstGte {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castLt}",			"AstGte {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOPV("AstNot   {$lhsp.castLtS, $lhsp.width1}",	"AstGteS{$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castLtS}",			"AstGteS{$lhsp->op1p(),$lhsp->op2p()}");
    TREEOPV("AstNot   {$lhsp.castLte, $lhsp.width1}",	"AstGt  {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castLte}",			"AstGt  {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOPV("AstNot   {$lhsp.castLteS, $lhsp.width1}",	"AstGtS {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castLteS}",		"AstGtS {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOPV("AstNot   {$lhsp.castGt, $lhsp.width1}",	"AstLte {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castGt}",			"AstLte {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOPV("AstNot   {$lhsp.castGtS, $lhsp.width1}",	"AstLteS{$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castGtS}",			"AstLteS{$lhsp->op1p(),$lhsp->op2p()}");
    TREEOPV("AstNot   {$lhsp.castGte, $lhsp.width1}",	"AstLt  {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castGte}",			"AstLt  {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOPV("AstNot   {$lhsp.castGteS, $lhsp.width1}",	"AstLtS {$lhsp->op1p(),$lhsp->op2p()}");
    TREEOP ("AstLogNot{$lhsp.castGteS}",		"AstLtS {$lhsp->op1p(),$lhsp->op2p()}");
    // Not common, but avoids compiler warnings about over shifting
    TREEOP ("AstShiftL{operandHugeShiftL(nodep)}",	"replaceZero(nodep)");
    TREEOP ("AstShiftR{operandHugeShiftR(nodep)}",	"replaceZero(nodep)");
    TREEOP ("AstShiftL{operandShiftOp(nodep)}",		"replaceShiftOp(nodep)");
    TREEOP ("AstShiftR{operandShiftOp(nodep)}",		"replaceShiftOp(nodep)");
    TREEOP ("AstShiftL{operandShiftShift(nodep)}",	"replaceShiftShift(nodep)");
    TREEOP ("AstShiftR{operandShiftShift(nodep)}",	"replaceShiftShift(nodep)");
    TREEOP ("AstWordSel{operandWordOOB(nodep)}",	"replaceZero(nodep)");
    ///=== Verilog operators
    // Comparison against 1'b0/1'b1; must be careful about widths.
    // These use Not, so must be Verilog only
    TREEOPV("AstEq    {$rhsp.width1, $lhsp.isZero,    $rhsp}",	"AstNot{$rhsp}");
    TREEOPV("AstEq    {$lhsp.width1, $lhsp, $rhsp.isZero}",	"AstNot{$lhsp}");
    TREEOPV("AstEq    {$rhsp.width1, $lhsp.isAllOnes, $rhsp}",	"replaceWRhs(nodep)");
    TREEOPV("AstEq    {$lhsp.width1, $lhsp, $rhsp.isAllOnes}",	"replaceWLhs(nodep)");
    TREEOPV("AstNeq   {$rhsp.width1, $lhsp.isZero,    $rhsp}",	"replaceWRhs(nodep)");
    TREEOPV("AstNeq   {$lhsp.width1, $lhsp, $rhsp.isZero}",	"replaceWLhs(nodep)");
    TREEOPV("AstNeq   {$rhsp.width1, $lhsp.isAllOnes, $rhsp}",	"AstNot{$rhsp}");
    TREEOPV("AstNeq   {$lhsp.width1, $lhsp, $rhsp.isAllOnes}",	"AstNot{$lhsp}");
    TREEOPV("AstLt    {$rhsp.width1, $lhsp.isZero,    $rhsp}",	"replaceWRhs(nodep)");  // Because not signed #s
    TREEOPV("AstGt    {$lhsp.width1, $lhsp, $rhsp.isZero}",	"replaceWLhs(nodep)");  // Because not signed #s
    // Useful for CONDs added around ARRAYSEL's in V3Case step
    TREEOPV("AstLte   {$lhsp->width()==$rhsp->width(), $rhsp.isAllOnes}", "replaceNum(nodep,1)");
    // Simplify reduction operators
    // This also gets &{...,0,....} => const 0  (Common for unused_ok signals)
    TREEOPV("AstRedXnor{$lhsp}",		"AstNot{AstRedXor{$lhsp}}");  // Just eliminate XNOR's
    TREEOPV("AstRedAnd{$lhsp, $lhsp.width1}",	"replaceWLhs(nodep)");
    TREEOPV("AstRedOr {$lhsp, $lhsp.width1}",	"replaceWLhs(nodep)");
    TREEOPV("AstRedXor{$lhsp, $lhsp.width1}",	"replaceWLhs(nodep)");
    TREEOPV("AstRedAnd{$lhsp->castConcat()}",	"AstAnd{AstRedAnd{$lhsp->castConcat()->lhsp()}, AstRedAnd{$lhsp->castConcat()->rhsp()}}");    // &{a,b} => {&a}&{&b}
    TREEOPV("AstRedOr {$lhsp->castConcat()}",	"AstOr {AstRedOr {$lhsp->castConcat()->lhsp()}, AstRedOr {$lhsp->castConcat()->rhsp()}}");    // |{a,b} => {|a}|{|b}
    TREEOPV("AstRedXor{$lhsp->castConcat()}",	"AstXor{AstRedXor{$lhsp->castConcat()->lhsp()}, AstRedXor{$lhsp->castConcat()->rhsp()}}");    // ^{a,b} => {^a}^{^b}
    TREEOPV("AstRedAnd{$lhsp->castExtend(), $lhsp->width()>$lhsp->castExtend()->lhsp()->width()}",	"replaceZero(nodep)");	// &{0,...} => 0    Prevents compiler limited range error
    TREEOPV("AstRedOr {$lhsp->castExtend()}",	"AstRedOr {$lhsp->castExtend()->lhsp()}");
    TREEOPV("AstRedXor{$lhsp->castExtend()}",	"AstRedXor{$lhsp->castExtend()->lhsp()}");
    TREEOPV("AstOneHot{$lhsp.width1}",		"replaceWLhs(nodep)");
    TREEOPV("AstOneHot0{$lhsp.width1}",		"replaceNum(nodep,1)");
    // Binary AND/OR is faster then logical and/or (usually)
    TREEOPV("AstLogAnd{$lhsp.width1, $rhsp.width1}", "AstAnd{$lhsp,$rhsp}");
    TREEOPV("AstLogOr {$lhsp.width1, $rhsp.width1}", "AstOr{$lhsp,$rhsp}");
    TREEOPV("AstLogNot{$lhsp.width1}",		    "AstNot{$lhsp}");
    // CONCAT(CONCAT({a},{b}),{c}) -> CONCAT({a},CONCAT({b},{c}))
    // CONCAT({const},CONCAT({const},{c})) -> CONCAT((constifiedCONC{const|const},{c}))
    TREEOPV("AstConcat{operandConcatMove(nodep)}",	"moveConcat(nodep)");
    TREEOPV("AstConcat{$lhsp.isZero, $rhsp}",		"replaceExtend(nodep, nodep->rhsp())");
    // Note can't simplify a extend{extends}, extends{extend}, as the sign bits end up in the wrong places
    TREEOPV("AstExtend {$lhsp.castExtend}",		"replaceExtend(nodep, nodep->lhsp()->castExtend()->lhsp())");
    TREEOPV("AstExtendS{$lhsp.castExtendS}",		"replaceExtend(nodep, nodep->lhsp()->castExtendS()->lhsp())");
    TREEOPV("AstReplicate{$lhsp, $rhsp.isOne, $lhsp->width()==nodep->width()}",	"replaceWLhs(nodep)");  // {1{lhs}}->lhs
    // Next rule because AUTOINST puts the width of bits in
    // to pins, even when the widths are exactly the same across the hierarchy.
    TREEOPV("AstSel{operandSelExtend(nodep)}",	"replaceWChild(nodep, nodep->fromp()->castExtend()->lhsp())");
    TREEOPV("AstSel{operandSelFull(nodep)}",	"replaceWChild(nodep, nodep->fromp())");
    TREEOPV("AstSel{$fromp.castSel}",		"replaceSelSel(nodep)");
    TREEOPV("AstSel{$fromp.castConst, $lsbp.castConst, $widthp.castConst, }",	"replaceConst(nodep)");
    TREEOPV("AstSel{$fromp.castConcat, $lsbp.castConst, $widthp.castConst, }",	"replaceSelConcat(nodep)");
    TREEOPV("AstSel{$fromp.castReplicate, $lsbp.castConst, $widthp.isOne, }",	"replaceSelReplicate(nodep)");
    // Conversions
    TREEOPV("AstLogIf {$lhsp, $rhsp}",		"AstLogOr{AstLogNot{$lhsp},$rhsp}");
    TREEOPV("AstLogIff{$lhsp, $rhsp}",		"AstLogNot{AstXor{$lhsp,$rhsp}}");

    // Possible futures:
    // (a?(b?y:x):y) -> (a&&!b)?x:y
    // (a?(b?x:y):y) -> (a&&b)?x:y
    // (a?x:(b?x:y)) -> (a||b)?x:y
    // (a?x:(b?y:x)) -> (a||!b)?x:y

    // Note we can't convert EqCase/NeqCase to Eq/Neq here because that would break 3'b1x1==3'b101

    //-----
    virtual void visit(AstNode* nodep, AstNUser*) {
	// Default: Just iterate
	if (m_required) {
	    nodep->v3error("Expecting expression to be constant, but can't convert a "
			   <<nodep->typeName()<<" to constant.");
	} else {
	    // Calculate the width of this operation
	    if (m_params && !nodep->width()) {
		V3Width::widthParams(nodep);
		V3Signed::signedParams(nodep);
	    }
	    nodep->iterateChildren(*this);
	}
    }

public:
    // CONSTUCTORS
    ConstVisitor(bool params, bool required, bool warn, bool cpp) {
	m_params = params; m_required = required; m_warn=warn; m_cpp=cpp;
	m_wremove = true;
	m_modp = NULL;
	m_scopep = NULL;
    }
    virtual ~ConstVisitor() {}
    void main(AstNode* nodep) {
	// Operate starting at a random place
	nodep->accept(*this);
    }
};

//######################################################################
// Const class functions

void V3Const::constifyParam(AstNode* nodep) {
    //if (debug()>0) nodep->dumpTree(cout,"  forceConPRE : ");
    if (!nodep->width()) {
	V3Width::widthParams(nodep);
	V3Signed::signedParams(nodep);
    }
    ConstVisitor visitor (true,true,false,false);
    visitor.main(nodep);
    // Because we do edits, nodep links may get trashed and core dump this.
    //if (debug()>0) nodep->dumpTree(cout,"  forceConDONE: ");
}

void V3Const::constifyAll(AstNetlist* nodep) {
    UINFO(2,__FUNCTION__<<": "<<endl);
    ConstVisitor visitor (false,false,false,false);
    visitor.main(nodep);
}

void V3Const::constifyAllLint(AstNode* nodep) {
    ConstVisitor visitor (false,false,true,false);
    visitor.main(nodep);
}

void V3Const::constifyCpp(AstNode* nodep) {
    ConstVisitor visitor (false,false,false,true);
    visitor.main(nodep);
}

void V3Const::constifyTree(AstNode* nodep) {
    ConstVisitor visitor (false,false,false,false);
    visitor.main(nodep);
}

