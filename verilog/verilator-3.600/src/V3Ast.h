// $Id$ //-*- C++ -*-
//*************************************************************************
// DESCRIPTION: Verilator: Ast node structure
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

#ifndef _V3AST_H_
#define _V3AST_H_ 1
#include "config.h"
#include "V3Error.h"
#include "V3Number.h"
#include <vector>

#include "V3Ast__gen_classes.h"	// From ./astgen
// Things like:
//   class V3AstNode;

//######################################################################

class AstType {
public:
#include "V3Ast__gen_types.h"	// From ./astgen
    // Above include has:
    //   enum en {...};
    //   const char* ascii() const {...};
    enum en m_e;
    inline AstType () {};
    inline AstType (en _e) : m_e(_e) {};
    explicit inline AstType (int _e) : m_e(static_cast<en>(_e)) {};
    operator en () const { return m_e; };
  };
  inline bool operator== (AstType lhs, AstType rhs) { return (lhs.m_e == rhs.m_e); }
  inline bool operator== (AstType lhs, AstType::en rhs) { return (lhs.m_e == rhs); }
  inline bool operator== (AstType::en lhs, AstType rhs) { return (lhs == rhs.m_e); }
  inline ostream& operator<<(ostream& os, AstType rhs) { return os<<rhs.ascii(); }

//######################################################################

class AstPragmaType {
public:
    enum en {
	COVERAGE_BLOCK_OFF,
	INLINE_MODULE,
	NO_INLINE_MODULE,
	PUBLIC_MODULE,
	PUBLIC_TASK
    };
    enum en m_e;
    inline AstPragmaType () {};
    inline AstPragmaType (en _e) : m_e(_e) {};
    explicit inline AstPragmaType (int _e) : m_e(static_cast<en>(_e)) {};
    operator en () const { return m_e; };
  };
  inline bool operator== (AstPragmaType lhs, AstPragmaType rhs) { return (lhs.m_e == rhs.m_e); }
  inline bool operator== (AstPragmaType lhs, AstPragmaType::en rhs) { return (lhs.m_e == rhs); }
  inline bool operator== (AstPragmaType::en lhs, AstPragmaType rhs) { return (lhs == rhs.m_e); }

//######################################################################

class AstCFuncType {
public:
    enum en {
	NORMAL,
	TRACE_INIT,
	TRACE_FULL,
	TRACE_CHANGE
    };
    enum en m_e;
    inline AstCFuncType () {};
    inline AstCFuncType (en _e) : m_e(_e) {};
    explicit inline AstCFuncType (int _e) : m_e(static_cast<en>(_e)) {};
    operator en () const { return m_e; };
  };
  inline bool operator== (AstCFuncType lhs, AstCFuncType rhs) { return (lhs.m_e == rhs.m_e); }
  inline bool operator== (AstCFuncType lhs, AstCFuncType::en rhs) { return (lhs.m_e == rhs); }
  inline bool operator== (AstCFuncType::en lhs, AstCFuncType rhs) { return (lhs == rhs.m_e); }

//######################################################################

class AstEdgeType {
public:
// REMEMBER to edit the strings below too
    enum en {
	// These must be in general -> most specific order, as we sort by it in AstSenTree::sortSenses()
	ILLEGAL,
	// Involving a variable
	ANYEDGE,	// Default for sensitivities; rip them out
	BOTHEDGE,	// POSEDGE | NEGEDGE
	POSEDGE,
	NEGEDGE,
	HIGHEDGE,	// Is high now (latches)
	LOWEDGE,	// Is low now (latches)
	// Not involving anything
	COMBO,		// Sensitive to all combo inputs to this block
	INITIAL,	// User initial statements
	SETTLE,		// Like combo but for initial wire resolutions after initial statement
	NEVER		// Never occurs (optimized away)
    };
    enum en m_e;
    bool clockedStmt() const {
	static const bool clocked[] = {
	    false, false, true, true, true, true, true,
	    false, false, false
	};
	return clocked[m_e];
    }
    AstEdgeType invert() const {
	switch (m_e) {
	case ANYEDGE:	return ANYEDGE;
	case BOTHEDGE:	return BOTHEDGE;
	case POSEDGE:	return NEGEDGE;
	case NEGEDGE:	return POSEDGE;
	case HIGHEDGE:	return LOWEDGE;
	case LOWEDGE:	return HIGHEDGE;
	default: UASSERT_STATIC(0,"Inverting bad edgeType()");
	};
	return AstEdgeType::ILLEGAL;
    }
    const char* ascii() const {
	static const char* names[] = {
	    "%E-edge", "ANY", "BOTH", "POS", "NEG", "HIGH", "LOW",
	    "COMBO","INITIAL","SETTLE","NEVER"
	};
	return names[m_e];
    };
    const char* verilogKwd() const {
	static const char* names[] = {
	    "%E-edge", "", "[both]", "posedge", "negedge", "[high]","[low]",
	    "/*AS*/","[initial]","[settle]","[never]"
	};
	return names[m_e];
    };
    inline AstEdgeType () {};
    inline AstEdgeType (en _e) : m_e(_e) {};
    explicit inline AstEdgeType (int _e) : m_e(static_cast<en>(_e)) {};
    operator en () const { return m_e; };
  };
  inline bool operator== (AstEdgeType lhs, AstEdgeType rhs) { return (lhs.m_e == rhs.m_e); }
  inline bool operator== (AstEdgeType lhs, AstEdgeType::en rhs) { return (lhs.m_e == rhs); }
  inline bool operator== (AstEdgeType::en lhs, AstEdgeType rhs) { return (lhs == rhs.m_e); }

//######################################################################

class AstAttrType {
public:
    enum en {
	BITS,
	RANGE_LSB,
	ARRAY_LSB,
	SCOPE_TEXT
    };
    enum en m_e;
    inline AstAttrType () {};
    inline AstAttrType (en _e) : m_e(_e) {};
    explicit inline AstAttrType (int _e) : m_e(static_cast<en>(_e)) {};
    operator en () const { return m_e; };
  };
  inline bool operator== (AstAttrType lhs, AstAttrType rhs) { return (lhs.m_e == rhs.m_e); }
  inline bool operator== (AstAttrType lhs, AstAttrType::en rhs) { return (lhs.m_e == rhs); }
  inline bool operator== (AstAttrType::en lhs, AstAttrType rhs) { return (lhs == rhs.m_e); }

//######################################################################

class AstVarType {
public:
    enum en {
	UNKNOWN,
	GPARAM,
	LPARAM,
	GENVAR,
	INTEGER,
	INPUT,
	OUTPUT,
	INOUT,
	SUPPLY0,
	SUPPLY1,
	WIRE,
	IMPLICIT,
	REG,
	TRIWIRE,
	BLOCKTEMP,
	MODULETEMP,
	STMTTEMP
    };
    enum en m_e;
    inline AstVarType () {};
    inline AstVarType (en _e) : m_e(_e) {};
    explicit inline AstVarType (int _e) : m_e(static_cast<en>(_e)) {};
    operator en () const { return m_e; };
    const char* ascii() const {
	static const char* names[] = {
	    "?","GPARAM","LPARAM","GENVAR",
	    "INTEGER","INPUT","OUTPUT","INOUT",
	    "SUPPLY0","SUPPLY1","WIRE","IMPLICIT","REG","TRIWIRE",
	    "BLOCKTEMP","MODULETEMP","STMTTEMP"};
	return names[m_e];};
  };
  inline bool operator== (AstVarType lhs, AstVarType rhs) { return (lhs.m_e == rhs.m_e); }
  inline bool operator== (AstVarType lhs, AstVarType::en rhs) { return (lhs.m_e == rhs); }
  inline bool operator== (AstVarType::en lhs, AstVarType rhs) { return (lhs == rhs.m_e); }
  inline ostream& operator<<(ostream& os, AstVarType rhs) { return os<<rhs.ascii(); }

//######################################################################

class AstBranchPred {
public:
    enum en {
	UNKNOWN=0,
	LIKELY,
	UNLIKELY,
	_ENUM_END
    };
    enum en m_e;
    // CONSTRUCTOR - note defaults to *UNKNOWN*
    inline AstBranchPred () : m_e(UNKNOWN) {};
    inline AstBranchPred (en _e) : m_e(_e) {};
    explicit inline AstBranchPred (int _e) : m_e(static_cast<en>(_e)) {};
    operator en () const { return m_e; };
    AstBranchPred invert() const {
	if (m_e==UNLIKELY) return LIKELY;
	else if (m_e==LIKELY) return UNLIKELY;
	else return m_e;
    }
    const char* ascii() const {
	static const char* names[] = {
	    "","VL_LIKELY","VL_UNLIKELY"};
	return names[m_e];};
  };
  inline bool operator== (AstBranchPred lhs, AstBranchPred rhs) { return (lhs.m_e == rhs.m_e); }
  inline bool operator== (AstBranchPred lhs, AstBranchPred::en rhs) { return (lhs.m_e == rhs); }
  inline bool operator== (AstBranchPred::en lhs, AstBranchPred rhs) { return (lhs == rhs.m_e); }
  inline ostream& operator<<(ostream& os, AstBranchPred rhs) { return os<<rhs.ascii(); }

//######################################################################
// AstNUser - Generic pointer base class for AST User nodes.
//	    - Also used to allow parameter passing up/down iterate calls

class WidthVP;
class LinkVP;
class OrderBlockNU;
class OrderVarNU;
class V3GraphVertex;
class V3SymTable;
struct AstNUser {
    AstNUser*	p() { return this; }	// So can take address of temporary: iterate(...,AstNUser(args).p())
    // Casters
    WidthVP*	c() { return ((WidthVP*)this); }
    LinkVP*	castLinkVP() { return ((LinkVP*)this); }
    V3SymTable*	castSymTable() { return ((V3SymTable*)this); }
    AstNode*	castNode() { return ((AstNode*)this); }
    OrderBlockNU* castOrderBlock() { return ((OrderBlockNU*)this); }
    OrderVarNU*	castOrderVar() { return ((OrderVarNU*)this); }
    V3GraphVertex* castGraphVertex() { return ((V3GraphVertex*)this); }
    inline int	castInt() {
	union { AstNUser* up; int ui; } u;
	u.up = this;
	return u.ui;
    }
    static inline AstNUser* fromInt (int i) {
	union { AstNUser* up; int ui; } u;
	u.up=0; u.ui=i;
	return u.up;
    }
};

//######################################################################
// AstNVisitor -- Allows new functions to be called on each node
// type without changing the base classes.  See "Modern C++ Design".

class AstNVisitor {
private:
    vector<AstNode*>	m_deleteps;	// Nodes to delete when we are finished
protected:
    friend class AstNode;
public:
    // Cleaning
    void pushDeletep(AstNode* nodep) {
	m_deleteps.push_back(nodep);
    }
    void doDeletes();
public:
    virtual ~AstNVisitor() {
	doDeletes();
    }
#include "V3Ast__gen_visitor.h"	// From ./astgen
    // Things like:
    //  virtual void visit(type*) = 0;
};

//######################################################################
// AstNRelinker -- Holds the state of a unlink so a new node can be
// added at the same point.

class AstNRelinker {
protected:
    friend class AstNode;
    enum RelinkWhatEn {
	RELINK_BAD, RELINK_NEXT, RELINK_OP1, RELINK_OP2, RELINK_OP3, RELINK_OP4
    };
    AstNode* m_oldp;	// The old node that was linked to this point in the tree
    AstNode* m_backp;
    RelinkWhatEn m_chg;
    AstNode** m_iterpp;
public:
    AstNRelinker() { m_backp=NULL; m_chg=RELINK_BAD; m_iterpp=NULL;}
    void relink(AstNode* newp);
    AstNode* oldp() const { return m_oldp; }
    void dump(ostream& str=cout);
};
inline ostream& operator<<(ostream& os, AstNRelinker& rhs) { rhs.dump(os); return os;}

//######################################################################
// V3Hash -- Node hashing for V3Combine

class V3Hash {
    // A hash of a tree of nodes, consisting of 8 bits with the number of nodes in the hash
    // and 24 bit value hash of relevant information about the node.
    // A value of 0 is illegal
    uint32_t	m_both;
    static const uint32_t M24 = ((1<<24)-1);
    void setBoth(uint32_t depth, uint32_t hshval) {
	if (depth==0) depth=1; if (depth>255) depth=255;
	m_both = (depth<<24) | (hshval & M24);
    }
public:
    // METHODS
    bool isIllegal() const { return m_both==0; }
    uint32_t fullValue() const { return m_both; }
    uint32_t depth() const { return (m_both >> 24) & 255; }
    uint32_t hshval() const { return m_both & M24; }
    // OPERATORS
    inline bool operator== (const V3Hash& rh) const { return m_both==rh.m_both; };
    inline bool operator< (const V3Hash& rh) const { return m_both<rh.m_both; };
    // CREATORS
    class Illegal {};		// for creator type-overload selection
    class FullValue {};		// for creator type-overload selection
    V3Hash(Illegal) { m_both=0; }
    // Saving and restoring inside a userp
    V3Hash(AstNUser* up) { m_both=up->castInt(); }
    V3Hash operator+= (const V3Hash& rh) {
	setBoth(depth()+rh.depth(), (hshval()*31+rh.hshval()));
	return *this; };
    // Creating from raw data (sameHash functions)
    V3Hash() { setBoth(1,0); }
    V3Hash(uint32_t val) { setBoth(1,val); }
    V3Hash(void* vp) {
	// It's just a hash, so we can shove a 64 bit pointer into a 32 bit bucket
	// On 32 bit systems, lower is always 0, but who cares?
	union { void* up; struct {uint32_t upper; uint32_t lower;} l;} u;
	u.l.upper=0; u.l.lower=0; u.up=vp;
	setBoth(1,u.l.upper^u.l.lower);
    }
    V3Hash(const string& name);
    V3Hash(V3Hash lh, V3Hash rh) {
	setBoth(1,lh.hshval()*31+rh.hshval());
    }
};
ostream& operator<<(ostream& os, V3Hash rhs);

//######################################################################
// AstNode -- Base type of all Ast types

class AstNode {
private:
    AstNode*	m_nextp;	// Next peer in the parent's list
    AstNode*	m_backp;	// Node that points to this one (via next/op1/op2/...)
    AstNode*	m_headtailp;	// When at begin/end of list, the opposite end of the list
    AstNode*	m_op1p;		// Generic pointer 1
    AstNode*	m_op2p;		// Generic pointer 2
    AstNode*	m_op3p;		// Generic pointer 3
    AstNode*	m_op4p;		// Generic pointer 4
    AstNode**	m_iterpp;	// Pointer to node iterating on, change it if we replace this node.

    AstNode*	m_clonep;	// Pointer to clone of/ source of this module (for *LAST* cloneTree() ONLY)
    int		m_cloneCnt;	// Mark of when userp was set
    static int	s_cloneCntGbl;	// Count of which userp is set

    FileLine*	m_fileline;	// Where it was declared
    uint64_t	m_editCount;	// When it was last edited
    static uint64_t s_editCntGbl;// Global edit counter
    static uint64_t s_editCntLast;// Global edit counter, last value for printing * near node #s

    // Attributes
    bool	m_signed;	// Node is signed
    int		m_width;	// Bit width of operation
    int		m_widthMin;	// If unsized, bitwidth of minimum implementation
    AstNUser*	m_userp;	// Pointer to any information the user iteration routine wants
    int		m_userCnt;	// Mark of when userp was set
    static int	s_userCntGbl;	// Count of which userp is set
    AstNUser*	m_user2p;	// Pointer to any information the user iteration routine wants
    int		m_user2Cnt;	// Mark of when userp was set
    static int	s_user2CntGbl;	// Count of which userp is set
    AstNUser*	m_user3p;	// Pointer to any information the user iteration routine wants
    int		m_user3Cnt;	// Mark of when userp was set
    static int	s_user3CntGbl;	// Count of which userp is set
    AstNUser*	m_user4p;	// Pointer to any information the user iteration routine wants
    int		m_user4Cnt;	// Mark of when userp was set
    static int	s_user4CntGbl;	// Count of which userp is set
    AstNUser*	m_user5p;	// Pointer to any information the user iteration routine wants
    int		m_user5Cnt;	// Mark of when userp was set
    static int	s_user5CntGbl;	// Count of which userp is set

    // METHODS
    void	op1p(AstNode* nodep) { m_op1p = nodep; if (nodep) nodep->m_backp = this; }
    void	op2p(AstNode* nodep) { m_op2p = nodep; if (nodep) nodep->m_backp = this; }
    void	op3p(AstNode* nodep) { m_op3p = nodep; if (nodep) nodep->m_backp = this; }
    void	op4p(AstNode* nodep) { m_op4p = nodep; if (nodep) nodep->m_backp = this; }

    void	init();	// initialize value of AstNode
    void	iterateListBackwards(AstNVisitor& v, AstNUser* vup=NULL);
    AstNode*	cloneTreeIter();
    AstNode*	cloneTreeIterList();
    void	checkTreeIter(AstNode* backp);
    void	checkTreeIterList(AstNode* backp);
    bool	sameTreeIter(AstNode* node2p, bool ignNext);
    void	deleteTreeIter();
    void	deleteNode();
    static void	relinkOneLink(AstNode*& pointpr, AstNode* newp);
    void	debugTreeChange(const char* prefix, int lineno, bool next);

protected:
    // CONSTUCTORS
    AstNode() {init(); }
    AstNode(FileLine* fileline) {init(); m_fileline = fileline; }
    virtual AstNode* clone() = 0;	// Generally, cloneTree/cloneNode is what you want
    virtual void cloneRelink() {}
    void 	cloneRelinkTree();

    // METHODS
    void	setOp1p(AstNode* newp);		// Set non-list-type op1 to non-list element
    void	setOp2p(AstNode* newp);		// Set non-list-type op2 to non-list element
    void	setOp3p(AstNode* newp);		// Set non-list-type op3 to non-list element
    void	setOp4p(AstNode* newp);		// Set non-list-type op4 to non-list element

    void	setNOp1p(AstNode* newp) { if (newp) setOp1p(newp); }
    void	setNOp2p(AstNode* newp) { if (newp) setOp2p(newp); }
    void	setNOp3p(AstNode* newp) { if (newp) setOp3p(newp); }
    void	setNOp4p(AstNode* newp) { if (newp) setOp4p(newp); }

    void	addOp1p(AstNode* newp);		// Append newp to end of op1
    void	addOp2p(AstNode* newp);		// Append newp to end of op2
    void	addOp3p(AstNode* newp);		// Append newp to end of op3
    void	addOp4p(AstNode* newp);		// Append newp to end of op4

    void	addNOp1p(AstNode* newp) { if (newp) addOp1p(newp); }
    void	addNOp2p(AstNode* newp) { if (newp) addOp2p(newp); }
    void	addNOp3p(AstNode* newp) { if (newp) addOp3p(newp); }
    void	addNOp4p(AstNode* newp) { if (newp) addOp4p(newp); }

    void	clonep(AstNode* nodep) { m_clonep=nodep; m_cloneCnt=s_cloneCntGbl; }
    static void	cloneClearTree() { s_cloneCntGbl++; UASSERT_STATIC(s_cloneCntGbl,"Rollover"); }

public:
    // ACCESSORS
    virtual AstType	type() const = 0;
    const char*	typeName() const { return type().ascii(); }
    AstNode*	nextp() const { return m_nextp; }
    AstNode*	backp() const { return m_backp; }
    AstNode*	op1p() const { return m_op1p; }
    AstNode*	op2p() const { return m_op2p; }
    AstNode*	op3p() const { return m_op3p; }
    AstNode*	op4p() const { return m_op4p; }
    AstNode*	clonep() const { return ((m_cloneCnt==s_cloneCntGbl)?m_clonep:NULL); }
    bool	brokeExists() const;

    // CONSTRUCTORS
    virtual ~AstNode();

    // CONSTANT ACCESSORS
    static int	instrCountBranch() { return 4; }	///< Instruction cycles to branch
    static int	instrCountDiv() { return 10; }		///< Instruction cycles to divide
    static int	instrCountLd() { return 2; }		///< Instruction cycles to load memory
    static int	instrCountMul() { return 3; }		///< Instruction cycles to multiply integers
    static int	instrCountPli() { return 20; }		///< Instruction cycles to call pli routines
    static int	instrCountCall() { return instrCountBranch()+10; }	///< Instruction cycles to call subroutine
    static int	instrCountTime() { return instrCountCall()+5; }		///< Instruction cycles to determine simulation time

    // ACCESSORS
    virtual string name() const { return ""; }
    virtual string verilogKwd() const { return ""; }
    string 	shortName() const;	// Name with __PVT__ removed for concatenating scopes
    static string prettyName(const string& namein);	// Name for printing out to the user
    string	prettyName() const { return prettyName(name()); }
    FileLine*	fileline() const { return m_fileline; }
    int		width() const { return m_width; }
    bool	width1() const { return width()==1; }
    int		widthWords() const { return VL_WORDS_I(width()); }
    int		widthMin() const { return m_widthMin?m_widthMin:m_width; }  // If sized, the size, if unsized the min digits to represent it
    int		widthPow2() const;
    int		widthInstrs() const { return isWide()?widthWords():1; }
    bool	widthSized() const { return !m_widthMin || m_widthMin==m_width; }
    void	width(int width, int sized) { m_width=width; m_widthMin=sized; }
    void	widthFrom(AstNode* fromp) { if (fromp) { m_width=fromp->m_width; m_widthMin=fromp->m_widthMin; }}
    void	widthSignedFrom(AstNode* fromp) { widthFrom(fromp); signedFrom(fromp); }
    void	signedFrom(AstNode* fromp) { if (fromp) { m_signed=fromp->m_signed; }}
    void	isSigned(bool flag) { m_signed=flag; }
    bool	isSigned() const { return m_signed; }
    bool	isQuad() const { return (width()>VL_WORDSIZE && width()<=VL_QUADSIZE); }
    bool	isWide() const { return (width()>VL_QUADSIZE); }

    int		user() const { return userp()->castInt(); }
    AstNUser*	userp() const { return ((m_userCnt==s_userCntGbl)?m_userp:NULL); }
    void	userp(void* userp) { m_userp=(AstNUser*)(userp); m_userCnt=s_userCntGbl; }
    void	user(int val) { userp(AstNUser::fromInt(val)); }
    static void	userClearTree() { s_userCntGbl++; UASSERT_STATIC(s_userCntGbl,"Rollover"); }  // Clear userp()'s across the entire tree
    int		user2() const { return user2p()->castInt(); }
    AstNUser*	user2p() const { return ((m_user2Cnt==s_user2CntGbl)?m_user2p:NULL); }
    void	user2p(void* userp) { m_user2p=(AstNUser*)(userp); m_user2Cnt=s_user2CntGbl; }
    void	user2(int val) { user2p(AstNUser::fromInt(val)); }
    static void	user2ClearTree() { s_user2CntGbl++; }  // Clear userp()'s across the entire tree
    int		user3() const { return user3p()->castInt(); }
    AstNUser*	user3p() const { return ((m_user3Cnt==s_user3CntGbl)?m_user3p:NULL); }
    void	user3p(void* userp) { m_user3p=(AstNUser*)(userp); m_user3Cnt=s_user3CntGbl; }
    void	user3(int val) { user3p(AstNUser::fromInt(val)); }
    static void	user3ClearTree() { s_user3CntGbl++; }  // Clear userp()'s across the entire tree
    int		user4() const { return user4p()->castInt(); }
    AstNUser*	user4p() const { return ((m_user4Cnt==s_user4CntGbl)?m_user4p:NULL); }
    void	user4p(void* userp) { m_user4p=(AstNUser*)(userp); m_user4Cnt=s_user4CntGbl; }
    void	user4(int val) { user4p(AstNUser::fromInt(val)); }
    static void	user4ClearTree() { s_user4CntGbl++; }  // Clear userp()'s across the entire tree
    int		user5() const { return user5p()->castInt(); }
    AstNUser*	user5p() const { return ((m_user5Cnt==s_user5CntGbl)?m_user5p:NULL); }
    void	user5p(void* userp) { m_user5p=(AstNUser*)(userp); m_user5Cnt=s_user5CntGbl; }
    void	user5(int val) { user5p(AstNUser::fromInt(val)); }
    static void	user5ClearTree() { s_user5CntGbl++; }  // Clear userp()'s across the entire tree

    uint64_t	editCount() const { return m_editCount; }
    void	editCountInc() { m_editCount = s_editCntGbl++; }
    static uint64_t	editCountLast() { return s_editCntLast; }
    static uint64_t	editCountGbl() { return s_editCntGbl; }
    static void		editCountSetLast() { s_editCntLast = editCountGbl(); }

    // ACCESSORS for specific types
    // Alas these can't be virtual or they break when passed a NULL
    bool	isZero();
    bool	isOne();
    bool	isNeqZero();
    bool	isAllOnes();
    bool	isAllOnesV();  // Verilog width rules apply

    // METHODS
    AstNode*	addNext(AstNode* newp);		// Returns this, adds to end of list
    AstNode*	addNextNull(AstNode* newp);	// Returns this, adds to end of list, NULL is OK
    void	addNextHere(AstNode* newp);	// Adds after speced node
    void	replaceWith(AstNode* newp);	// Replace current node in tree with new node
    void	v3errorEnd(ostringstream& str);
    virtual void dump(ostream& str=cout);
    AstNode*	unlinkFrBack(AstNRelinker* linkerp=NULL);  // Unlink this from whoever points to it.
    AstNode*	unlinkFrBackWithNext(AstNRelinker* linkerp=NULL);  // Unlink this from whoever points to it, keep entire next list with unlinked node
    void	relink(AstNRelinker* linkerp);	// Generally use linker->relink() instead
    void	cloneRelinkNode() { cloneRelink(); }

    // METHODS - Iterate on a tree
    AstNode*	cloneTree(bool cloneNextLink);
    bool	sameTree(AstNode* node2p);	// Does tree of this == node2p?
    void	deleteTree();	// Always deletes the next link
    void	checkTree();  // User Interface version
    void	dumpPtrs(ostream& str=cout);
    void	dumpTree(ostream& str=cout, const string& indent="    ", int maxDepth=0);
    void	dumpTreeAndNext(ostream& str=cout, const string& indent="    ", int maxDepth=0);
    void	dumpTreeFile(const string& filename, bool append=false);

    // METHODS - queries
    virtual bool isSplittable() const { return true; }	// Else a $display, etc, that must be ordered with other displays
    virtual bool isGateOptimizable() const { return true; }	// Else a AstTime etc that can't be pushed out
    virtual bool isSubstOptimizable() const { return true; }	// Else a AstTime etc that can't be substituted out
    virtual bool isPredictOptimizable() const { return true; }	// Else a AstTime etc which output can't be predicted from input
    virtual bool isOutputter() const { return false; }	// Else creates output or exits, etc, not unconsumed
    virtual bool isUnlikely() const { return false; }	// Else $stop or similar statement which means an above IF statement is unlikely to be taken
    virtual int  instrCount() const { return 0; }
    virtual V3Hash sameHash() const { return V3Hash(V3Hash::Illegal()); }  // Not a node that supports it
    virtual bool same(AstNode* otherp) const { return true; }
    virtual bool broken() const { return false; }
    virtual bool emitWordForm() { return false; }

    // INVOKERS
    virtual void accept(AstNVisitor& v, AstNUser* vup=NULL) = 0;
    void	iterate(AstNVisitor& v, AstNUser* vup=NULL) { this->accept(v,vup); } 	  // Does this; excludes following this->next
    void	iterateAndNext(AstNVisitor& v, AstNUser* vup=NULL);
    void	iterateAndNextIgnoreEdit(AstNVisitor& v, AstNUser* vup=NULL);
    void	iterateChildren(AstNVisitor& v, AstNUser* vup=NULL);  // Excludes following this->next
    void	iterateChildrenBackwards(AstNVisitor& v, AstNUser* vup=NULL);  // Excludes following this->next

    // CONVERSION
    AstNode*	castNode() { return this; }
#include "V3Ast__gen_interface.h"	// From ./astgen
    // Things like:
    //  AstAlways*	castAlways();
};

inline ostream& operator<<(ostream& os, AstNode* rhs) { rhs->dump(os); return os;}
inline void AstNRelinker::relink(AstNode* newp) { newp->AstNode::relink(this); }

//######################################################################
//######################################################################
//=== AstNode* : Derived generic node types

struct AstNodeMath : public AstNode {
    // Math -- anything that's part of an expression tree
    AstNodeMath(FileLine* fl)
	: AstNode(fl) {}
    virtual ~AstNodeMath() {}
    // METHODS
    virtual string emitVerilog() = 0;  /// Format string for verilog writing; see V3EmitV
    virtual string emitOperator() = 0;
    virtual string emitSimpleOperator() { return ""; }
    virtual bool cleanOut() = 0; // True if output has extra upper bits zero
};

struct AstNodeTermop : public AstNodeMath {
    // Terminal operator -- a operator with no "inputs"
    AstNodeTermop(FileLine* fl)
	: AstNodeMath(fl) {}
    virtual ~AstNodeTermop() {}
};

struct AstNodeUniop : public AstNodeMath {
    // Unary math
    AstNodeUniop(FileLine* fl, AstNode* lhsp)
	: AstNodeMath(fl) {
	if (lhsp) widthSignedFrom(lhsp);
	setOp1p(lhsp); }
    virtual ~AstNodeUniop() {}
    AstNode*	lhsp() 	const { return op1p()->castNode(); }
    // METHODS
    virtual void numberOperate(V3Number& out, const V3Number& lhs) = 0; // Set out to evaluation of a AstConst'ed lhs
    virtual bool cleanLhs() = 0;
    virtual bool sizeMattersLhs() = 0; // True if output result depends on lhs size
    virtual int instrCount()	const { return widthInstrs(); }
    virtual V3Hash sameHash() const { return V3Hash(); }
    virtual bool same(AstNode*) const { return true; }
};

struct AstNodeBiop : public AstNodeMath {
    // Binary math
    AstNodeBiop(FileLine* fl, AstNode* lhs, AstNode* rhs)
	: AstNodeMath(fl) {
	setOp1p(lhs); setOp2p(rhs); }
    virtual ~AstNodeBiop() {}
    AstNode*	lhsp() 	const { return op1p()->castNode(); }
    AstNode*	rhsp() 	const { return op2p()->castNode(); }
    void	lhsp(AstNode* nodep)  { return setOp1p(nodep); }
    void	rhsp(AstNode* nodep)  { return setOp2p(nodep); }
    // METHODS
    virtual void numberOperate(V3Number& out, const V3Number& lhs, const V3Number& rhs) = 0; // Set out to evaluation of a AstConst'ed
    virtual bool cleanLhs() = 0; // True if LHS must have extra upper bits zero
    virtual bool cleanRhs() = 0; // True if RHS must have extra upper bits zero
    virtual bool sizeMattersLhs() = 0; // True if output result depends on lhs size
    virtual bool sizeMattersRhs() = 0; // True if output result depends on rhs size
    virtual bool signedFlavor() const { return false; }	// Signed flavor of nodes with both flavors?
    virtual int instrCount()	const { return widthInstrs(); }
    virtual V3Hash sameHash() const { return V3Hash(); }
    virtual bool same(AstNode*) const { return true; }
};

struct AstNodeTriop : public AstNodeMath {
    // Trinary math
    AstNodeTriop(FileLine* fl, AstNode* lhs, AstNode* rhs, AstNode* ths)
	: AstNodeMath(fl) {
	setOp1p(lhs); setOp2p(rhs); setOp3p(ths); }
    virtual ~AstNodeTriop() {}
    AstNode*	lhsp() 	const { return op1p()->castNode(); }
    AstNode*	rhsp() 	const { return op2p()->castNode(); }
    AstNode*	thsp() 	const { return op3p()->castNode(); }
    void	lhsp(AstNode* nodep)  { return setOp1p(nodep); }
    void	rhsp(AstNode* nodep)  { return setOp2p(nodep); }
    void	thsp(AstNode* nodep)  { return setOp3p(nodep); }
    // METHODS
    virtual void numberOperate(V3Number& out, const V3Number& lhs, const V3Number& rhs, const V3Number& ths) = 0; // Set out to evaluation of a AstConst'ed
    virtual bool cleanLhs() = 0; // True if LHS must have extra upper bits zero
    virtual bool cleanRhs() = 0; // True if RHS must have extra upper bits zero
    virtual bool cleanThs() = 0; // True if THS must have extra upper bits zero
    virtual bool sizeMattersLhs() = 0; // True if output result depends on lhs size
    virtual bool sizeMattersRhs() = 0; // True if output result depends on rhs size
    virtual bool sizeMattersThs() = 0; // True if output result depends on ths size
    virtual int instrCount()	const { return widthInstrs(); }
    virtual V3Hash sameHash() const { return V3Hash(); }
    virtual bool same(AstNode*) const { return true; }
};

struct AstNodeBiCom : public AstNodeBiop {
    // Binary math with commutative properties
    AstNodeBiCom(FileLine* fl, AstNode* lhs, AstNode* rhs)
	: AstNodeBiop(fl, lhs, rhs) {}
    virtual ~AstNodeBiCom() {}
};

struct AstNodeBiComAsv : public AstNodeBiCom {
    // Binary math with commutative & associative properties
    AstNodeBiComAsv(FileLine* fl, AstNode* lhs, AstNode* rhs)
	: AstNodeBiCom(fl, lhs, rhs) {}
    virtual ~AstNodeBiComAsv() {}
};
struct AstNodeCond : public AstNodeTriop {
    AstNodeCond(FileLine* fl, AstNode* condp, AstNode* expr1p, AstNode* expr2p)
	: AstNodeTriop(fl, condp, expr1p, expr2p) {
	if (expr1p) widthSignedFrom(expr1p);
	else if (expr2p) widthSignedFrom(expr2p);
    }
    virtual ~AstNodeCond() {}
    virtual void numberOperate(V3Number& out, const V3Number& lhs, const V3Number& rhs, const V3Number& ths) {
	if (lhs.isNeqZero()) out.opAssign(rhs); else out.opAssign(ths); }
    AstNode*	condp() 	const { return op1p()->castNode(); }	// op1 = Condition
    AstNode*	expr1p() 	const { return op2p()->castNode(); }	// op2 = If true...
    AstNode*	expr2p() 	const { return op3p()->castNode(); }	// op3 = If false...
    virtual string emitVerilog() { return "%k(%l %k? %r %k: %t)"; }
    virtual string emitOperator() { return "VL_COND"; }
    virtual bool cleanOut() { return false; } // clean if e1 & e2 clean
    virtual bool cleanLhs() { return true; }
    virtual bool cleanRhs() { return false; } virtual bool cleanThs() { return false; } // Propagates up
    virtual bool sizeMattersLhs() { return false; } virtual bool sizeMattersRhs() { return false; }
    virtual bool sizeMattersThs() { return false; }
    virtual int instrCount()	const { return instrCountBranch(); }
};

struct AstNodePreSel : public AstNode {
    // Something that becomes a AstSel
    AstNodePreSel(FileLine* fl, AstNode* lhs, AstNode* rhs, AstNode* ths)
	: AstNode(fl) {
	setOp1p(lhs); setOp2p(rhs); setNOp3p(ths); }
    virtual ~AstNodePreSel() {}
    AstNode*	lhsp() 	const { return op1p()->castNode(); }
    AstNode*	rhsp() 	const { return op2p()->castNode(); }
    AstNode*	thsp() 	const { return op3p()->castNode(); }
    void	lhsp(AstNode* nodep)  { return setOp1p(nodep); }
    void	rhsp(AstNode* nodep)  { return setOp2p(nodep); }
    void	thsp(AstNode* nodep)  { return setOp3p(nodep); }
    // METHODS
    virtual V3Hash sameHash() const { return V3Hash(); }
    virtual bool same(AstNode*) const { return true; }
};

struct AstNodeStmt : public AstNode {
    // Statement -- anything that's directly under a function
    AstNodeStmt(FileLine* fl)
	: AstNode(fl) {}
    virtual ~AstNodeStmt() {}
    // METHODS
};

struct AstNodeAssign : public AstNodeStmt {
    AstNodeAssign(FileLine* fl, AstNode* lhsp, AstNode* rhsp)
	: AstNodeStmt(fl) {
	setOp1p(rhsp); setOp2p(lhsp);
	if (lhsp) widthSignedFrom(lhsp);
    }
    virtual ~AstNodeAssign() {}
    virtual AstNode*	cloneType(AstNode* lhsp, AstNode* rhsp)=0;	// Clone single node, just get same type back.
    // So iteration hits the RHS which is "earlier" in execution order, it's op1, not op2
    AstNode* rhsp()		const { return op1p()->castNode(); }	// op1 = Assign from
    AstNode* lhsp()		const { return op2p()->castNode(); }	// op2 = Assign to
    void rhsp(AstNode* np) { setOp1p(np); }
    void lhsp(AstNode* np) { setOp2p(np); }
    virtual bool cleanRhs() { return true; }
    virtual int  instrCount() const { return widthInstrs(); }
    virtual V3Hash sameHash() const { return V3Hash(); }
    virtual bool same(AstNode*) const { return true; }
};

struct AstNodeFor : public AstNodeStmt {
    AstNodeFor(FileLine* fileline, AstNode* initsp, AstNode* condp,
	       AstNode* assignsp, AstNode* bodysp)
	: AstNodeStmt(fileline) {
	addNOp1p(initsp); setOp2p(condp); addNOp3p(assignsp); addNOp4p(bodysp);
    }
    virtual ~AstNodeFor() {}
    AstNode*	initsp()	const { return op1p()->castNode(); }	// op1= initial statement
    AstNode*	condp()		const { return op2p()->castNode(); }	// op2= condition to continue
    AstNode*	assignsp()	const { return op3p()->castNode(); }	// op3= final statements
    AstNode*	bodysp()	const { return op4p()->castNode(); }	// op4= body of loop
    virtual bool isGateOptimizable() const { return false; }
    virtual bool isPredictOptimizable() const { return false; }
    virtual int  instrCount() const { return instrCountBranch(); }
    virtual V3Hash sameHash() const { return V3Hash(); }
    virtual bool same(AstNode* samep) const { return true; }
};

struct AstNodeIf : public AstNodeStmt {
private:
    AstBranchPred	m_branchPred;	// Branch prediction as taken/untaken?
public:
    AstNodeIf(FileLine* fl, AstNode* condp, AstNode* ifsp, AstNode* elsesp)
	: AstNodeStmt(fl) {
	setOp1p(condp); addNOp2p(ifsp); addNOp3p(elsesp);
    }
    virtual ~AstNodeIf() {}
    AstNode*	condp()		const { return op1p(); }	// op1 = condition
    AstNode*	ifsp()		const { return op2p(); }	// op2 = list of true statements
    AstNode*	elsesp()	const { return op3p(); }	// op3 = list of false statements
    void	condp(AstNode* newp)		{ setOp1p(newp); }
    void	addIfsp(AstNode* newp)		{ addOp2p(newp); }
    void	addElsesp(AstNode* newp)	{ addOp3p(newp); }
    virtual bool isGateOptimizable() const { return false; }
    virtual int  instrCount() const { return instrCountBranch(); }
    virtual V3Hash sameHash() const { return V3Hash(); }
    virtual bool same(AstNode* samep) const { return true; }
    void    branchPred(AstBranchPred flag) { m_branchPred = flag; }
    AstBranchPred branchPred() const { return m_branchPred; }
};

struct AstNodeCase : public AstNodeStmt {
    AstNodeCase(FileLine* fl, AstNode* exprp, AstNode* casesp)
	: AstNodeStmt(fl) {
	setOp1p(exprp); addNOp2p(casesp);
    }
    virtual ~AstNodeCase() {}
    virtual int   instrCount()	const { return instrCountBranch(); }
    AstNode*	  exprp()	const { return op1p()->castNode(); }	// op1 = case condition <expression>
    AstCaseItem*  itemsp()	const { return op2p()->castCaseItem(); }  // op2 = list of case expressions
    AstNode*	  notParallelp() const { return op3p()->castNode(); }	// op3 = assertion code for non-full case's
    void addItemsp(AstNode* nodep) { addOp2p(nodep); }
    void addNotParallelp(AstNode* nodep) { setOp3p(nodep); }
};

class AstNodeVarRef : public AstNodeMath {
    // A AstVarRef or AstVarXRef
private:
    bool	m_lvalue;	// Left hand side assignment
    AstVar*	m_varp;		// [AfterLink] Pointer to variable itself
    AstVarScope* m_varScopep;	// Varscope for hierarchy
    string	m_name;		// Name of variable
    string	m_hiername;	// Scope converted into name-> for emitting
    bool	m_hierThis;	// Hiername points to "this" function
public:
    AstNodeVarRef(FileLine* fl, const string& name, bool lvalue)
	: AstNodeMath(fl), m_lvalue(lvalue), m_varp(NULL), m_varScopep(NULL),
	  m_name(name), m_hierThis(false) {
    }
    AstNodeVarRef(FileLine* fl, const string& name, AstVar* varp, bool lvalue)
	: AstNodeMath(fl), m_lvalue(lvalue), m_varp(varp), m_varScopep(NULL),
	  m_name(name), m_hierThis(false) {
	// May have varp==NULL
	if (m_varp) widthSignedFrom((AstNode*)m_varp);
    }
    virtual ~AstNodeVarRef() {}
    virtual bool broken() const;
    virtual int instrCount() const { return widthInstrs(); }
    virtual void cloneRelink();
    virtual string name()	const { return m_name; }		// * = Var name
    void 	name(const string& name) 	{ m_name = name; }
    bool	lvalue() const { return m_lvalue; }
    void	lvalue(bool lval) { m_lvalue=lval; }  // Avoid using this; Set in constructor
    AstVar*	varp() const { return m_varp; }				// [After Link] Pointer to variable
    void  	varp(AstVar* varp) { m_varp=varp; }
    AstVarScope*	varScopep() const { return m_varScopep; }
    void	varScopep(AstVarScope* varscp) { m_varScopep=varscp; }
    string hiername() const { return m_hiername; }
    void hiername(const string& hn) { m_hiername = hn; }
    bool hierThis() const { return m_hierThis; }
    void hierThis(bool flag) { m_hierThis = flag; }
};

class AstNodePli : public AstNodeStmt {
    string	m_text;
public:
    AstNodePli(FileLine* fl, const string& text, AstNode* exprsp)
	: AstNodeStmt(fl), m_text(text) {
	addNOp1p(exprsp); }
    virtual ~AstNodePli() {}
    virtual string name()	const { return m_text; }
    virtual int instrCount()	const { return instrCountPli(); }
    AstNode* exprsp()		const { return op1p()->castNode(); }	// op1 = Expressions to output
    string 	text()		const { return m_text; }		// * = Text to display
    void text(const string& text) { m_text=text; }
    // op2p,op3p... used by AstDisplay
};

struct AstNodeText : public AstNode {
private:
    string	m_text;
public:
    // Node that simply puts text into the output stream
    AstNodeText(FileLine* fileline, const string& textp)
	: AstNode(fileline) {
	m_text = textp;	// Copy it
    }
    virtual ~AstNodeText() {}
    const string& text() const { return m_text; }
    virtual V3Hash sameHash() const { return V3Hash(text()); }
    virtual bool same(AstNode* samep) const {
	return text()==samep->castNodeText()->text(); }
};

struct AstNodeSel : public AstNodeBiop {
    // Single bit range extraction, perhaps with non-constant selection or array selection
    AstNodeSel(FileLine* fl, AstNode* fromp, AstNode* bitp)
	:AstNodeBiop(fl, fromp, bitp) {}
    AstNode* fromp()		const { return op1p()->castNode(); }	// op1 = Extracting what (NULL=TBD during parsing)
    AstNode* bitp()		const { return op2p()->castNode(); }	// op2 = Msb selection expression
    int	     bitConst()	const;
};

//######################################################################
// Tasks/functions common handling

struct AstNodeFTask : public AstNode {
private:
    string	m_name;		// Name of task
    bool	m_taskPublic;	// Public task
public:
    // Node that simply puts name into the output stream
    AstNodeFTask(FileLine* fileline, const string& name, AstNode* stmtsp)
	: AstNode(fileline)
	, m_name(name), m_taskPublic(false) {
	addNOp3p(stmtsp);
    }
    virtual ~AstNodeFTask() {}
    virtual void dump(ostream& str=cout);
    virtual string name()	const { return m_name; }		// * = Var name
    // {AstFunc only} op1 = Range output variable
    // op3 = Statements/Ports/Vars
    void 	name(const string& name) 	{ m_name = name; }
    AstNode*	stmtsp() 	const { return op3p()->castNode(); }	// op1 = List of statements
    void	addStmtsp(AstNode* nodep) { addOp3p(nodep); }
    void	taskPublic(bool flag) { m_taskPublic=flag; }
    bool	taskPublic() const { return m_taskPublic; }
};

struct AstNodeFTaskRef : public AstNode {
    // A reference to a task (or function)
private:
    AstNodeFTask*	m_taskp;	// [AfterLink] Pointer to task referenced
    string		m_name;		// Name of variable
    string		m_dotted;	// Dotted part of scope to task or ""
    string		m_inlinedDots;	// Dotted hiearchy flattened out
public:
    AstNodeFTaskRef(FileLine* fl, const string& name, const string& dotted, AstNode* pinsp)
	:AstNode(fl)
	, m_taskp(NULL), m_name(name), m_dotted(dotted) {
	addNOp1p(pinsp);
    }
    virtual ~AstNodeFTaskRef() {}
    virtual bool broken() const { return m_taskp && !m_taskp->brokeExists(); }
    virtual void cloneRelink() { if (m_taskp && m_taskp->clonep()) {
	m_taskp = m_taskp->clonep()->castNodeFTask();
    }}
    virtual void dump(ostream& str=cout);
    virtual string name()	const { return m_name; }		// * = Var name
    string	dotted()	const { return m_dotted; }		// * = Scope name or ""
    string	prettyDotted() const { return prettyName(dotted()); }
    string	inlinedDots() const { return m_inlinedDots; }
    void	inlinedDots(const string& flag) { m_inlinedDots = flag; }
    AstNodeFTask*	taskp() const { return m_taskp; }		// [After Link] Pointer to variable
    void  	taskp(AstNodeFTask* taskp) { m_taskp=taskp; }
    // op1 = Pin interconnection list
    AstNode*	pinsp() 	const { return op1p()->castNode(); }
    void addPinsp(AstNode* nodep) { addOp1p(nodep); }
};

//######################################################################

#include "V3AstNodes.h"

#include "V3Ast__gen_impl.h"	// From ./astgen
// Things like:
//  inline AstAlways*	AstNode::castAlways() {	  return dynamic_cast<AstAlways*>(this);}

//######################################################################
// Inline ACCESSORS

inline bool AstNode::isZero()     { return (this->castConst() && this->castConst()->num().isEqZero()); }
inline bool AstNode::isNeqZero()  { return (this->castConst() && this->castConst()->num().isNeqZero()); }
inline bool AstNode::isOne()      { return (this->castConst() && this->castConst()->num().isEqOne()); }
inline bool AstNode::isAllOnes()  { return (this->castConst() && this->castConst()->num().isEqAllOnes(this->width())); }
inline bool AstNode::isAllOnesV() { return (this->castConst() && this->castConst()->num().isEqAllOnes(this->widthMin())); }

#endif // Guard
