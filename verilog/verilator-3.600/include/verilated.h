// $Id$ -*- C++ -*-
//*************************************************************************
//
// Copyright 2003-2006 by Wilson Snyder. This program is free software; you can
// redistribute it and/or modify it under the terms of either the GNU
// General Public License or the Perl Artistic License.
//
// Verilator is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
//*************************************************************************
///
/// \file
/// \brief Verilator: Common include for all Verilated C files
///
///	This file is included automatically by Verilator at the top of
///	all C++ files it generates.  It contains standard macros and
///	classes required by the Verilated code.
///
/// Code available from: http://www.veripool.com/verilator
///
//*************************************************************************


#ifndef _VERILATED_H_
#define _VERILATED_H_ 1 ///< Header Guard

#include "verilatedos.h"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
using namespace std;

//=========================================================================
// Basic types

typedef uint8_t  CData;		///< Verilated data, 1-8 bits
typedef uint16_t SData;		///< Verilated data, 9-16 bits
typedef uint32_t IData;		///< Verilated data, 17-32 bits
typedef vluint64_t QData;	///< Verilated data, 33-64 bits
typedef uint32_t   WData;	///< Verilated data, >64 bits, as an array

typedef const WData* WDataInP;	///< Array input to a function
typedef       WData* WDataOutP;	///< Array output from a function

class SpTraceVcd;
class SpTraceVcdCFile;

//=========================================================================
/// Base class for all Verilated module classes

class VerilatedModule {
private:
    const char*		m_namep;	///< Module name
    VerilatedModule();				///< N/A, always use named constructor below
    VerilatedModule(const VerilatedModule& );	///< N/A, no copying modules
public:
    VerilatedModule(const char* namep);	///< Create module with given hierarchy name
    ~VerilatedModule();
    const char* name() const { return m_namep; }	///< Return name of module
};

//=========================================================================
// Declare nets

#ifndef VL_ST_SIG
# define VL_ST_SIG8(name, msb,lsb)	CData name		///< Declare signal, 1-8 bits
# define VL_ST_SIG16(name, msb,lsb)	SData name		///< Declare signal, 9-16 bits
# define VL_ST_SIG64(name, msb,lsb)	QData name		///< Declare signal, 33-64 bits
# define VL_ST_SIG(name, msb,lsb)	IData name		///< Declare signal, 17-32 bits
# define VL_ST_SIGW(name,msb,lsb,words)	WData name[words]	///< Declare signal, 65+ bits
#endif
#ifndef VL_SIG
# define VL_SIG8(name, msb,lsb)		CData name		///< Declare signal, 1-8 bits
# define VL_SIG16(name, msb,lsb)	SData name		///< Declare signal, 9-16 bits
# define VL_SIG64(name, msb,lsb)	QData name		///< Declare signal, 33-64 bits
# define VL_SIG(name, msb,lsb)		IData name		///< Declare signal, 17-32 bits
# define VL_SIGW(name, msb,lsb, words)	WData name[words]	///< Declare signal, 65+ bits
# define VL_IN8(name, msb,lsb)		CData name		///< Declare input signal, 1-8 bits
# define VL_IN16(name, msb,lsb)		SData name		///< Declare input signal, 9-16 bits
# define VL_IN64(name, msb,lsb)		QData name		///< Declare input signal, 33-64 bits
# define VL_IN(name, msb,lsb)		IData name		///< Declare input signal, 17-32 bits
# define VL_INW(name, msb,lsb, words)	WData name[words]	///< Declare input signal, 65+ bits
# define VL_INOUT8(name, msb,lsb)	CData name		///< Declare bidir signal, 1-8 bits
# define VL_INOUT16(name, msb,lsb)	SData name		///< Declare bidir signal, 9-16 bits
# define VL_INOUT64(name, msb,lsb)	QData name		///< Declare bidir signal, 33-64 bits
# define VL_INOUT(name, msb,lsb)	IData name		///< Declare bidir signal, 17-32 bits
# define VL_INOUTW(name, msb,lsb, words) WData name[words]	///< Declare bidir signal, 65+ bits
# define VL_OUT8(name, msb,lsb)		CData name		///< Declare output signal, 1-8 bits
# define VL_OUT16(name, msb,lsb)	SData name		///< Declare output signal, 9-16 bits
# define VL_OUT64(name, msb,lsb)	QData name		///< Declare output signal, 33-64bits
# define VL_OUT(name, msb,lsb)		IData name		///< Declare output signal, 17-32 bits
# define VL_OUTW(name, msb,lsb, words)	WData name[words]	///< Declare output signal, 65+ bits

# define VL_PIN_NOP(instname,pin,port)	///< Connect a pin, ala SP_PIN
# define VL_CELL(instname,type)		///< Declare a cell, ala SP_CELL

/// Declare a module, ala SC_MODULE
# define VL_MODULE(modname)		struct modname : public VerilatedModule

/// Constructor, ala SC_CTOR
# define VL_CTOR(modname)		modname(const char* __VCname="")

/// Constructor declaration for C++, ala SP_CTOR_IMPL
# define VL_CTOR_IMP(modname)		modname::modname(const char* __VCname) : VerilatedModule(__VCname)

/// Constructor declaration for SystemC, ala SP_CTOR_IMPL
# define VL_SC_CTOR_IMP(modname)	modname::modname(sc_module_name)

#endif

//===========================================================================
/// Verilator global static information class

struct Verilated {
    // Extern Vars
    // Below two are used as bool, but having as uint32_t avoids conversion time
private:
    static int  	s_randReset;		///< Random reset: 0=all 0s, 1=all 1s, 2=random
    static int		s_debug;		///< See accessors... only when VL_DEBUG set
    static bool		s_calcUnusedSigs;	///< Waves file on, need all signals calculated
    static bool		s_gotFinish;		///< A $finish statement executed
    static bool		s_assertOn;		///< Assertions are enabled
public:
    /// Select initial value of otherwise uninitialized signals.
    ////
    /// 0 = Set to zeros
    /// 1 = Set all bits to one
    /// 2 = Randomize all bits
    static void randReset(int val) { s_randReset=val; }
    static int  randReset() { return s_randReset; }	///< Return randReset value

    /// Enable debug of internal verilated code
    static inline void debug(int level) { s_debug = level; }
#ifdef VL_DEBUG
    static inline int  debug() { return s_debug; }	///< Return debug value
#else
    static inline int  debug() { return 0; }		///< Constant 0 debug, so C++'s optimizer rips up
#endif
    /// Internal: Create a new module name by concatenating two strings
    static const char* catName(const char* n1, const char* n2); // Returns new'ed data
    /// Enable calculation of unused signals 
    static void calcUnusedSigs(bool flag) { s_calcUnusedSigs=flag; }
    static bool calcUnusedSigs() { return s_calcUnusedSigs; }	///< Return calcUnusedSigs value
    /// Did the simulation $finish?
    static void gotFinish(bool flag) { s_gotFinish=flag; }
    static bool gotFinish() { return s_gotFinish; }	///< Return if got a $finish
    /// Allow traces to at some point be enabled (disables some optimizations)
    static void traceEverOn(bool flag) {
	if (flag) { calcUnusedSigs(flag); }
    }
    /// Enable/disable assertions
    static void assertOn(bool flag) { s_assertOn=flag; }
    static bool assertOn() { return s_assertOn; }
};    

//=========================================================================
// Extern functions -- User may override -- See verilated.cpp

/// Routine to call for $finish
extern void vl_finish (const char* filename, int linenum, const char* hier);
/// Routine to call for $stop
extern void vl_stop   (const char* filename, int linenum, const char* hier);
/// Routine to call for a couple of fatal messages
extern void vl_fatal  (const char* filename, int linenum, const char* hier,
		       const char* msg);

//=========================================================================
// Extern functions -- Init time only, so slow is fine

extern IData  VL_RAND_RESET_I(int obits);	///< Random reset a signal
extern QData  VL_RAND_RESET_Q(int obits);	///< Random reset a signal
extern WDataOutP VL_RAND_RESET_W(int obits, WDataOutP outwp);	///< Random reset a signal
extern WDataOutP VL_ZERO_RESET_W(int obits, WDataOutP outwp);	///< Zero reset a signal

/// Return string with formated Verilog value
extern const char* VL_VALUE_FORMATTED_W(int obits, char fmt, bool drop0, WDataInP lwp);
extern const char* VL_VALUE_FORMATTED_Q(int obits, char fmt, bool drop0, QData ld);
inline const char* VL_VALUE_FORMATTED_I(int obits, char fmt, bool drop0, IData ld) {
    return VL_VALUE_FORMATTED_Q(obits,fmt,drop0,ld);
}

/// File I/O
extern QData VL_FOPEN_WI(int fnwords, WDataInP filename, IData mode);
extern QData VL_FOPEN_QI(QData filename, IData mode);
inline QData VL_FOPEN_II(IData filename, IData mode) { return VL_FOPEN_QI(filename,mode); }

//=========================================================================
// Base macros

/// Return true if data[bit] set
#define VL_BITISSET_I(data,bit) (data & (1UL<<VL_BITBIT_I(bit)))
#define VL_BITISSET_Q(data,bit) (data & (VL_ULL(1)<<VL_BITBIT_Q(bit)))
#define VL_BITISSET_W(data,bit) (data[VL_BITWORD_I(bit)] & (1UL<<VL_BITBIT_I(bit)))

/// Create two 32-bit words from quadword
#define VL_SET_WQ(decl,data)	{ decl[0]=(data); decl[1]=((data)>>VL_WORDSIZE); }

// Use a union to avoid cast-to-different-size warnings
/// Return FILE* from QData
static inline FILE*  VL_CVT_Q_FP(QData lhs) { union { FILE* fp; QData q; } u; u.q=lhs; return u.fp; }
/// Return QData from FILE*
static inline QData  VL_CVT_FP_Q(FILE* fp) { union { FILE* fp; QData q; } u; u.q=0; u.fp=fp; return u.q; }

// Sign extend such that if MSB set, we get ffff_ffff, else 0s
// (Requires clean input)
#define VL_SIGN_I(nbits,lhs)      ((lhs) >> VL_BITBIT_I((nbits) - 1UL))
#define VL_SIGN_Q(nbits,lhs)      ((lhs) >> VL_BITBIT_Q((nbits) - VL_ULL(1)))
#define VL_SIGNONES_I(nbits,lhs)  (-(VL_SIGN_I(nbits,lhs)))

// Sign bit extended up to MSB, doesn't include unsigned portion
// Optimization bug in GCC 3.3 returns different bitmasks to later states for
static inline IData  VL_EXTENDSIGN_I(int lbits, IData lhs) { return (-((lhs)&(1UL<<(lbits-1)))); }
static inline QData  VL_EXTENDSIGN_Q(int lbits, QData lhs) { return (-((lhs)&(VL_ULL(1)<<(lbits-1)))); }

//=========================================================================
// Pli macros

/// Return current simulation time
#if defined(SYSTEMC_VERSION) && (SYSTEMC_VERSION>20011000)
# define VL_TIME_I(ign) ((IData)sc_time_stamp().to_default_time_units())
# define VL_TIME_Q(ign) ((QData)sc_time_stamp().to_default_time_units())
#else
# define VL_TIME_I(ign) ((IData)sc_time_stamp())
# define VL_TIME_Q(ign) ((QData)sc_time_stamp())
extern double sc_time_stamp();
#endif

/// Evaluate expression if debug enabled
#ifdef VL_DEBUG
# define VL_DEBUG_IF(text) {if (VL_UNLIKELY(Verilated::debug())) {text}}
#else
# define VL_DEBUG_IF(text)
#endif

/// Collect coverage analysis for this line
#ifndef SP_AUTO_COVER3
# define SP_AUTO_COVER3(what,file,line)
#endif


//=========================================================================
// Functional macros/routines
// These all take the form
//	VL_func_IW(bits,bits,op,op)
//	VL_func_WW(bits,bits,out,op,op)
// The I/W indicates if it's a integer or wide for the output and each operand.
// The bits indicate the bit width of the output and each operand.
// If wide output, a temporary storage location is specified.

//===================================================================
// SETTING OPERATORS

// Output clean
// EMIT_RULE: VL_CLEAN:  oclean=clean; obits=lbits;
#define VL_CLEAN_II(obits,lbits,lhs) ((lhs) & VL_MASK_I(obits))
#define VL_CLEAN_QQ(obits,lbits,lhs) ((lhs) & VL_MASK_Q(obits))

// EMIT_RULE: VL_ASSIGNCLEAN:  oclean=clean; obits==lbits;
#define VL_ASSIGNCLEAN_W(obits,owp,lwp) VL_CLEAN_WW(obits,obits,owp,lwp)
static inline WDataOutP VL_CLEAN_WW(int obits, int, WDataOutP owp, WDataInP lwp){
    int words = VL_WORDS_I(obits);
    for (int i=0; (i < (words-1)); i++) owp[i] = lwp[i];
    owp[words-1] = lwp[words-1] & VL_MASK_I(obits);
    return(owp);
}

// EMIT_RULE: VL_ASSIGN:  oclean=rclean; obits==lbits;
// For now, we always have a clean rhs.
// Note: If a ASSIGN isn't clean, use VL_ASSIGNCLEAN instead to do the same thing.
static inline WDataOutP VL_ASSIGN_W(int obits, WDataOutP owp,WDataInP lwp){
    int words = VL_WORDS_I(obits);
    for (int i=0; i < words; i++) owp[i] = lwp[i];
    return(owp);
}

// EMIT_RULE: VL_ASSIGNBIT:  rclean=clean;
static inline void VL_ASSIGNBIT_II(int, int bit, CData& lhsr, IData rhs) {
    lhsr = ((lhsr & ~(1UL<<VL_BITBIT_I(bit)))
	    | (rhs<<VL_BITBIT_I(bit)));
}
static inline void VL_ASSIGNBIT_II(int, int bit, SData& lhsr, IData rhs) {
    lhsr = ((lhsr & ~(1UL<<VL_BITBIT_I(bit)))
	    | (rhs<<VL_BITBIT_I(bit)));
}
static inline void VL_ASSIGNBIT_II(int, int bit, IData& lhsr, IData rhs) {
    lhsr = ((lhsr & ~(1UL<<VL_BITBIT_I(bit)))
	    | (rhs<<VL_BITBIT_I(bit)));
}
static inline void VL_ASSIGNBIT_QI(int, int bit, QData& lhsr, QData rhs) {
    lhsr = ((lhsr & ~(VL_ULL(1)<<VL_BITBIT_Q(bit)))
	    | (rhs<<VL_BITBIT_Q(bit)));
}
static inline void VL_ASSIGNBIT_WI(int, int bit, WDataOutP owp, IData rhs) {
    IData orig = owp[VL_BITWORD_I(bit)];
    owp[VL_BITWORD_I(bit)] = (orig & ~(1UL<<VL_BITBIT_I(bit))
			    | (rhs<<VL_BITBIT_I(bit)));
}
// Alternative form that is an instruction faster when rhs is constant one.
static inline void VL_ASSIGNBIT_IO(int, int bit, CData& lhsr, IData) {
    lhsr = (lhsr | (1UL<<VL_BITBIT_I(bit)));
}
static inline void VL_ASSIGNBIT_IO(int, int bit, SData& lhsr, IData) {
    lhsr = (lhsr | (1UL<<VL_BITBIT_I(bit)));
}
static inline void VL_ASSIGNBIT_IO(int, int bit, IData& lhsr, IData) {
    lhsr = (lhsr | (1UL<<VL_BITBIT_I(bit)));
}
static inline void VL_ASSIGNBIT_QO(int, int bit, QData& lhsr, IData) {
    lhsr = (lhsr | (VL_ULL(1)<<VL_BITBIT_Q(bit)));
}
static inline void VL_ASSIGNBIT_WO(int, int bit, WDataOutP owp, IData) {
    IData orig = owp[VL_BITWORD_I(bit)];
    owp[VL_BITWORD_I(bit)] = (orig |  (1UL<<VL_BITBIT_I(bit)));
}

//===================================================================
// SYSTEMC OPERATORS
// Copying verilog format to/from systemc integers and bit vectors.

#define VL_ASSIGN_IS(obits,vvar,svar) { (vvar) = VL_CLEAN_II((obits),(obits),(svar).read()); }
#define VL_ASSIGN_SI(obits,svar,vvar) { (svar).write(vvar); }

#define VL_ASSIGN_QSQ(obits,vvar,svar) { (vvar) = VL_CLEAN_QQ((obits),(obits),(svar).read()); }
#define VL_ASSIGN_SQQ(obits,svar,vvar) { (svar).write(vvar); }

#define VL_ASSIGN_SQ(obits,svar,rd) { \
    sc_bv<obits> _bvtemp; \
    _bvtemp.set_word(0,rd); \
    _bvtemp.set_word(1,rd>>VL_WORDSIZE); \
    svar.write(_bvtemp); \
}
#define VL_ASSIGN_QS(obits,od,svar) { \
    od = (((QData)svar.read().get_word(1))<<VL_WORDSIZE | svar.read().get_word(0)) \
	& VL_MASK_Q(obits); \
}

#define VL_ASSIGN_SW(obits,svar,rwp) { \
    sc_bv<obits> _bvtemp; \
    for (int i=0; i < VL_WORDS_I(obits); i++) _bvtemp.set_word(i,rwp[i]); \
    svar.write(_bvtemp); \
}
#define VL_ASSIGN_WS(obits,owp,svar) { \
    int words = VL_WORDS_I(obits); \
    for (int i=0; i < words; i++) owp[i] = svar.read().get_word(i); \
    owp[words-1] &= VL_MASK_I(obits); \
}

//===================================================================
// Extending sizes

// CAREFUL, we're width changing, so obits!=lbits

// Right must be clean because otherwise size increase would pick up bad bits
// EMIT_RULE: VL_EXTEND:  oclean=clean; rclean==clean;
#define VL_EXTEND_II(obits,lbits,lhs) ((lhs))
#define VL_EXTEND_QI(obits,lbits,lhs) ((QData)(lhs))
#define VL_EXTEND_QQ(obits,lbits,lhs) ((lhs))

static inline WDataOutP VL_EXTEND_WI(int obits, int, WDataOutP owp, IData ld) {
    // Note for extracts that obits != lbits
    owp[0] = ld;
    for (int i=1; i < VL_WORDS_I(obits); i++) owp[i] = 0;
    return(owp);
}
static inline WDataOutP VL_EXTEND_WQ(int obits, int, WDataOutP owp, QData ld) {
    VL_SET_WQ(owp,ld);
    for (int i=2; i < VL_WORDS_I(obits); i++) owp[i] = 0;
    return(owp);
}
static inline WDataOutP VL_EXTEND_WW(int obits, int lbits, WDataOutP owp, WDataInP lwp) {
    for (int i=0; i < VL_WORDS_I(lbits); i++) owp[i] = lwp[i];
    for (int i=VL_WORDS_I(lbits); i < VL_WORDS_I(obits); i++) owp[i] = 0;
    return(owp);
}

// EMIT_RULE: VL_EXTENDS:  oclean=*dirty*; obits=lbits;
// Sign extension; output dirty
static inline IData VL_EXTENDS_II(int, int lbits, IData lhs) {
    return VL_EXTENDSIGN_I(lbits,lhs) | lhs;
}
static inline QData VL_EXTENDS_QI(int, int lbits, QData lhs/*Q_as_need_extended*/) {
    return VL_EXTENDSIGN_Q(lbits,lhs) | lhs;
}
static inline QData VL_EXTENDS_QQ(int, int lbits, QData lhs) {
    return VL_EXTENDSIGN_Q(lbits,lhs) | lhs;
}

static inline WDataOutP VL_EXTENDS_WI(int obits, int lbits, WDataOutP owp, IData ld) {
    IData sign = VL_SIGNONES_I(lbits,ld);
    owp[0] = ld | sign & ~VL_MASK_I(lbits);
    for (int i=1; i < VL_WORDS_I(obits); i++) owp[i] = sign;
    return(owp);
}
static inline WDataOutP VL_EXTENDS_WQ(int obits, int lbits, WDataOutP owp, QData ld) {
    VL_SET_WQ(owp,ld);
    IData sign = VL_SIGNONES_I(lbits,owp[1]);
    owp[1] |= sign & ~VL_MASK_I(lbits);
    for (int i=2; i < VL_WORDS_I(obits); i++) owp[i] = sign;
    return(owp);
}
static inline WDataOutP VL_EXTENDS_WW(int obits, int lbits, WDataOutP owp, WDataInP lwp) {
    for (int i=0; i < VL_WORDS_I(lbits)-1; i++) owp[i] = lwp[i];
    int lmsw=VL_WORDS_I(lbits)-1;
    IData sign = VL_SIGNONES_I(lbits,lwp[lmsw]);
    owp[lmsw] = lwp[lmsw] | sign & ~VL_MASK_I(lbits);
    for (int i=VL_WORDS_I(lbits); i < VL_WORDS_I(obits); i++) owp[i] = sign;
    return(owp);
}

//===================================================================
// REDUCTION OPERATORS

// EMIT_RULE: VL_REDAND:  oclean=clean; lclean==clean; obits=1;
#define VL_REDAND_II(obits,lbits,lhs) (lhs == VL_MASK_I(lbits))
#define VL_REDAND_IQ(obits,lbits,lhs) (lhs == VL_MASK_Q(lbits))
static inline IData VL_REDAND_IW(int, int lbits, WDataInP lwp) {
    int words = VL_WORDS_I(lbits);
    IData combine=lwp[0];
    for (int i=1; i < words-1; i++) combine &= lwp[i];
    combine &= ~VL_MASK_I(lbits) | lwp[words-1];
    return ((~combine)==0);
}

// EMIT_RULE: VL_REDOR:  oclean=clean; lclean==clean; obits=1;
#define VL_REDOR_I(lhs) (lhs!=0)
#define VL_REDOR_Q(lhs) (lhs!=0)
static inline IData VL_REDOR_W(int words, WDataInP lwp) {
    IData equal=0;
    for (int i=0; i < words; i++) equal |= lwp[i];
    return(equal!=0);
}

// EMIT_RULE: VL_REDXOR:  oclean=dirty; obits=1;
static inline IData VL_REDXOR_W(int words, WDataInP lwp) {
    IData r = lwp[0];
    for (int i=1; i < words; i++) r ^= lwp[i];
    r=(r^(r>>1));
    r=(r^(r>>2));
    r=(r^(r>>4));
    r=(r^(r>>8));
    r=(r^(r>>16));
    return r;
}
static inline IData VL_REDXOR_2(IData r) {
    r=(r^(r>>1));
    return r;
}
static inline IData VL_REDXOR_4(IData r) {
    r=(r^(r>>1)); r=(r^(r>>2));
    return r;
}
static inline IData VL_REDXOR_8(IData r) {
    r=(r^(r>>1)); r=(r^(r>>2)); r=(r^(r>>4));
    return r;
}
static inline IData VL_REDXOR_16(IData r) {
    r=(r^(r>>1)); r=(r^(r>>2)); r=(r^(r>>4)); r=(r^(r>>8));
    return r;
}
static inline IData VL_REDXOR_32(IData r) {
    r=(r^(r>>1)); r=(r^(r>>2)); r=(r^(r>>4)); r=(r^(r>>8)); r=(r^(r>>16));
    return r;
}
static inline IData VL_REDXOR_64(QData r) {
    r=(r^(r>>1)); r=(r^(r>>2)); r=(r^(r>>4)); r=(r^(r>>8)); r=(r^(r>>16)); r=(r^(r>>32));
    return r;
}

// EMIT_RULE: VL_COUNTONES_II:  oclean = false; lhs clean
static inline IData VL_COUNTONES_I(IData lhs) {
    IData r = lhs - ((lhs >> 1) & 033333333333) - ((lhs >> 2) & 011111111111);
    r = (r + (r>>3)) & 030707070707;
    r = (r + (r>>6));
    r = (r + (r>>12) + (r>>24)) & 077;
    return r;
}
static inline IData VL_COUNTONES_Q(QData lhs) {
    return VL_COUNTONES_I(lhs) + VL_COUNTONES_I(lhs>>32);
}
static inline IData VL_COUNTONES_W(int words, WDataInP lwp) {
    IData r = 0;
    for (int i=0; (i < words); i++) r+=VL_COUNTONES_I(lwp[i]);
    return r;
}

static inline IData VL_ONEHOT_I(IData lhs) {
    return ((lhs & (lhs-1))==0 & lhs!=0);
}
static inline IData VL_ONEHOT_Q(QData lhs) {
    return ((lhs & (lhs-1))==0 & lhs!=0);
}
static inline IData VL_ONEHOT_W(int words, WDataInP lwp) {
    IData one=0;
    for (int i=0; (i < words); i++) {
	if (lwp[i]) {
	    if (one) return 0;
	    one = 1;
	    if (lwp[i] & (lwp[i]-1)) return 0;
	}
    }
    return one;
}

static inline IData VL_ONEHOT0_I(IData lhs) {
    return ((lhs & (lhs-1))==0);
}
static inline IData VL_ONEHOT0_Q(QData lhs) {
    return ((lhs & (lhs-1))==0);
}
static inline IData VL_ONEHOT0_W(int words, WDataInP lwp) {
    bool one=false;
    for (int i=0; (i < words); i++) {
	if (lwp[i]) {
	    if (one) return 0;
	    one = true;
	    if (lwp[i] & (lwp[i]-1)) return 0;
	}
    }
    return 1;
}

//===================================================================
// SIMPLE LOGICAL OPERATORS

// EMIT_RULE: VL_AND:  oclean=lclean||rclean; obits=lbits; lbits==rbits;
static inline WDataOutP VL_AND_W(int words, WDataOutP owp,WDataInP lwp,WDataInP rwp){
    for (int i=0; (i < words); i++) owp[i] = (lwp[i] & rwp[i]);
    return(owp);
}
// EMIT_RULE: VL_OR:   oclean=lclean&&rclean; obits=lbits; lbits==rbits;
static inline WDataOutP VL_OR_W(int words, WDataOutP owp,WDataInP lwp,WDataInP rwp){
    for (int i=0; (i < words); i++) owp[i] = (lwp[i] | rwp[i]);
    return(owp);
}
// EMIT_RULE: VL_CHANGEXOR:  oclean=1; obits=32; lbits==rbits;
static inline IData VL_CHANGEXOR_W(int words, WDataInP lwp,WDataInP rwp){
    IData od;
    for (int i=0; (i < words); i++) od |= (lwp[i] ^ rwp[i]);
    return(od);
}
// EMIT_RULE: VL_XOR:  oclean=lclean&&rclean; obits=lbits; lbits==rbits;
static inline WDataOutP VL_XOR_W(int words, WDataOutP owp,WDataInP lwp,WDataInP rwp){
    for (int i=0; (i < words); i++) owp[i] = (lwp[i] ^ rwp[i]);
    return(owp);
}
// EMIT_RULE: VL_XNOR:  oclean=dirty; obits=lbits; lbits==rbits;
static inline WDataOutP VL_XNOR_W(int words, WDataOutP owp,WDataInP lwp,WDataInP rwp){
    for (int i=0; (i < words); i++) owp[i] = (lwp[i] ^ ~rwp[i]);
    return(owp);
}
// EMIT_RULE: VL_NOT:  oclean=dirty; obits=lbits;
static inline WDataOutP VL_NOT_W(int words, WDataOutP owp,WDataInP lwp) {
    for (int i=0; i < words; i++) owp[i] = ~(lwp[i]);
    return(owp);
}

//=========================================================================
// Logical comparisons

// EMIT_RULE: VL_EQ:  oclean=clean; lclean==clean; rclean==clean; obits=1; lbits==rbits;
// EMIT_RULE: VL_NEQ: oclean=clean; lclean==clean; rclean==clean; obits=1; lbits==rbits;
// EMIT_RULE: VL_LT:  oclean=clean; lclean==clean; rclean==clean; obits=1; lbits==rbits;
// EMIT_RULE: VL_GT:  oclean=clean; lclean==clean; rclean==clean; obits=1; lbits==rbits;
// EMIT_RULE: VL_GTE: oclean=clean; lclean==clean; rclean==clean; obits=1; lbits==rbits;
// EMIT_RULE: VL_LTE: oclean=clean; lclean==clean; rclean==clean; obits=1; lbits==rbits;
#define VL_NEQ_W(words,lwp,rwp)		(!VL_EQ_W(words,lwp,rwp))
#define VL_LT_W(words,lwp,rwp)		(_VL_CMP_W(words,lwp,rwp)<0)
#define VL_LTE_W(words,lwp,rwp)		(_VL_CMP_W(words,lwp,rwp)<=0)
#define VL_GT_W(words,lwp,rwp)		(_VL_CMP_W(words,lwp,rwp)>0)
#define VL_GTE_W(words,lwp,rwp)		(_VL_CMP_W(words,lwp,rwp)>=0)

// Output clean, <lhs> AND <rhs> MUST BE CLEAN
static inline IData VL_EQ_W(int words, WDataInP lwp, WDataInP rwp) {
    int nequal=0;
    for (int i=0; (i < words); i++) nequal |= (lwp[i] ^ rwp[i]);
    return(nequal==0);
}

// Internal usage
static inline int _VL_CMP_W(int words, WDataInP lwp, WDataInP rwp) {
    for (int i=words-1; i>=0; --i) {
	if (lwp[i] > rwp[i]) return 1;
	if (lwp[i] < rwp[i]) return -1;
    }
    return(0); // ==
}

#define VL_LTS_IWW(obits,lbits,rbbits,lwp,rwp)		(_VL_CMPS_W(lbits,lwp,rwp)<0)
#define VL_LTES_IWW(obits,lbits,rbits,lwp,rwp)		(_VL_CMPS_W(lbits,lwp,rwp)<=0)
#define VL_GTS_IWW(obits,lbits,rbits,lwp,rwp)		(_VL_CMPS_W(lbits,lwp,rwp)>0)
#define VL_GTES_IWW(obits,lbits,rbits,lwp,rwp)		(_VL_CMPS_W(lbits,lwp,rwp)>=0)

static inline IData VL_GTS_III(int, int lbits, int, IData lhs, IData rhs) {
    // For lbits==32, this becomes just a single instruction, otherwise ~5.
    // GCC 3.3.4 sign extension bugs on AMD64 architecture force us to use quad logic
    vlsint64_t lhs_signed = VL_EXTENDS_QQ(64, lbits, lhs); //Q for gcc
    vlsint64_t rhs_signed = VL_EXTENDS_QQ(64, lbits, rhs); //Q for gcc
    return lhs_signed > rhs_signed;
}
static inline IData VL_GTS_IQQ(int, int lbits, int, QData lhs, QData rhs) {
    vlsint64_t lhs_signed = VL_EXTENDS_QQ(64, lbits, lhs);
    vlsint64_t rhs_signed = VL_EXTENDS_QQ(64, lbits, rhs);
    return lhs_signed > rhs_signed;
}

static inline IData VL_GTES_III(int, int lbits, int, IData lhs, IData rhs) {
    vlsint64_t lhs_signed = VL_EXTENDS_QQ(64, lbits, lhs); //Q for gcc
    vlsint64_t rhs_signed = VL_EXTENDS_QQ(64, lbits, rhs); //Q for gcc
    return lhs_signed >= rhs_signed;
}
static inline IData VL_GTES_IQQ(int, int lbits, int, QData lhs, QData rhs) {
    vlsint64_t lhs_signed = VL_EXTENDS_QQ(64, lbits, lhs);
    vlsint64_t rhs_signed = VL_EXTENDS_QQ(64, lbits, rhs);
    return lhs_signed >= rhs_signed;
}

static inline IData VL_LTS_III(int, int lbits, int, IData lhs, IData rhs) {
    vlsint64_t lhs_signed = VL_EXTENDS_QQ(64, lbits, lhs); //Q for gcc
    vlsint64_t rhs_signed = VL_EXTENDS_QQ(64, lbits, rhs); //Q for gcc
    return lhs_signed < rhs_signed;
}
static inline IData VL_LTS_IQQ(int, int lbits, int, QData lhs, QData rhs) {
    vlsint64_t lhs_signed = VL_EXTENDS_QQ(64, lbits, lhs);
    vlsint64_t rhs_signed = VL_EXTENDS_QQ(64, lbits, rhs);
    return lhs_signed < rhs_signed;
}

static inline IData VL_LTES_III(int, int lbits, int, IData lhs, IData rhs) {
    vlsint64_t lhs_signed = VL_EXTENDS_QQ(64, lbits, lhs); //Q for gcc
    vlsint64_t rhs_signed = VL_EXTENDS_QQ(64, lbits, rhs); //Q for gcc
    return lhs_signed <= rhs_signed;
}
static inline IData VL_LTES_IQQ(int, int lbits, int, QData lhs, QData rhs) {
    vlsint64_t lhs_signed = VL_EXTENDS_QQ(64, lbits, lhs);
    vlsint64_t rhs_signed = VL_EXTENDS_QQ(64, lbits, rhs);
    return lhs_signed <= rhs_signed;
}

static inline int _VL_CMPS_W(int lbits, WDataInP lwp, WDataInP rwp) {
    int words = VL_WORDS_I(lbits);
    int i=words-1;
    // We need to flip sense if negative comparison
    IData lsign = VL_SIGN_I(lbits,lwp[i]);
    IData rsign = VL_SIGN_I(lbits,rwp[i]);
    if (!lsign && rsign) return  1; // + > -
    if (lsign && !rsign) return -1; // - < +
    for (; i>=0; --i) {
	if (lwp[i] > rwp[i]) return 1;
	if (lwp[i] < rwp[i]) return -1;
    }
    return(0); // ==
}

//=========================================================================
// Math

// EMIT_RULE: VL_MUL:    oclean=dirty; lclean==clean; rclean==clean;
// EMIT_RULE: VL_DIV:    oclean=dirty; lclean==clean; rclean==clean;
// EMIT_RULE: VL_MODDIV: oclean=dirty; lclean==clean; rclean==clean;
#define VL_DIV_I(lhs,rhs)	(((rhs)==0)?0:(lhs)/(rhs))
#define VL_DIV_Q(lhs,rhs)	(((rhs)==0)?0:(lhs)/(rhs))
#define VL_MODDIV_I(lhs,rhs)	(((rhs)==0)?0:(lhs)%(rhs))
#define VL_MODDIV_Q(lhs,rhs)	(((rhs)==0)?0:(lhs)%(rhs))

static inline WDataOutP VL_ADD_W(int words, WDataOutP owp,WDataInP lwp,WDataInP rwp){
    QData carry = 0;
    for (int i=0; i<words; i++) {
	carry = carry + (QData)(lwp[i]) + (QData)(rwp[i]);
	owp[i] = (carry & VL_ULL(0xffffffff));
	carry = (carry >> VL_ULL(32)) & VL_ULL(0xffffffff);
    }
    return(owp);
}

static inline WDataOutP VL_SUB_W(int words, WDataOutP owp,WDataInP lwp,WDataInP rwp){
    QData carry = 0;
    for (int i=0; i<words; i++) {
	carry = carry + (QData)(lwp[i]) + (QData)(~rwp[i]);
	if (i==0) carry++;  // Negation of temp2
	owp[i] = (carry & VL_ULL(0xffffffff));
	carry = (carry >> VL_ULL(32)) & VL_ULL(0xffffffff);
    }
    return(owp);
}

// Optimization bug in GCC 2.96 and presumably all-pre GCC 3 versions need this workaround
#if 1 //defined(__GNUC__) && __GNUC__ < 3
static inline IData  VL_UNARYMIN_I(IData data) { return -data; }
static inline QData  VL_UNARYMIN_Q(QData data) { return -data; }
#else
# define VL_UNARYMIN_I(IData data) (-(data))
# define VL_UNARYMIN_Q(QData data) (-(data))
#endif

static inline WDataOutP VL_UNARYMIN_W(int words, WDataOutP owp,WDataInP lwp){
    QData carry = 0;
    for (int i=0; i<words; i++) {
	carry = carry + (QData)(~lwp[i]);
	if (i==0) carry++;  // Negation of temp2
	owp[i] = (carry & VL_ULL(0xffffffff));
	carry = (carry >> VL_ULL(32)) & VL_ULL(0xffffffff);
    }
    return(owp);
}

static inline WDataOutP VL_MUL_W(int words, WDataOutP owp,WDataInP lwp,WDataInP rwp){
    for (int i=0; i<words; i++) owp[i] = 0;
    for (int lword=0; lword<words; lword++) {
	for (int rword=0; rword<words; rword++) {
	    QData mul = (QData)(lwp[lword]) * (QData)(rwp[rword]);
	    for (int qword=lword+rword; qword<words; qword++) {
		mul += (QData)(owp[qword]);
		owp[qword] = (mul & VL_ULL(0xffffffff));
		mul = (mul >> VL_ULL(32)) & VL_ULL(0xffffffff);
	    }
	}
    }
    // Last output word is dirty
    return(owp);
}

static inline IData VL_MULS_III(int,int lbits,int, IData lhs,IData rhs) {
    vlsint32_t lhs_signed = VL_EXTENDS_II(32, lbits, lhs);
    vlsint32_t rhs_signed = VL_EXTENDS_II(32, lbits, rhs);
    return lhs_signed * rhs_signed;
}
static inline QData VL_MULS_QQQ(int,int lbits,int, QData lhs,QData rhs) {
    vlsint64_t lhs_signed = VL_EXTENDS_QQ(64, lbits, lhs);
    vlsint64_t rhs_signed = VL_EXTENDS_QQ(64, lbits, rhs);
    return lhs_signed * rhs_signed;
}

static inline WDataOutP VL_MULS_WWW(int,int lbits,int, WDataOutP owp,WDataInP lwp,WDataInP rwp){
    int words = VL_WORDS_I(lbits);
    IData lwstore[VL_MULS_MAX_WORDS]; // Fixed size, as MSVC++ doesn't allow [words] here
    IData rwstore[VL_MULS_MAX_WORDS];
    WDataInP lwusp = lwp;
    WDataInP rwusp = rwp;
    IData lneg = VL_SIGN_I(lbits,lwp[words-1]);
    if (lneg) { // Negate lhs
	lwusp = lwstore;
	VL_UNARYMIN_W(words, lwstore, lwp);
	lwstore[words-1] &= VL_MASK_I(lbits);  // Clean it
    }
    IData rneg = VL_SIGN_I(lbits,rwp[words-1]);
    if (rneg) { // Negate rhs
	rwusp = rwstore;
	VL_UNARYMIN_W(words, rwstore, rwp);
	rwstore[words-1] &= VL_MASK_I(lbits);  // Clean it
    }
    VL_MUL_W(words,owp,lwusp,rwusp);
    owp[words-1] &= VL_MASK_I(lbits);  // Clean.  Note it's ok for the multiply to overflow into the sign bit
    if ((lneg ^ rneg) & 1) {      // Negate output  (not using UNARYMIN, as owp==lwp)
	QData carry = 0;
	for (int i=0; i<words; i++) {
	    carry = carry + (QData)(~owp[i]);
	    if (i==0) carry++;  // Negation of temp2
	    owp[i] = (carry & VL_ULL(0xffffffff));
	    carry = (carry >> VL_ULL(32)) & VL_ULL(0xffffffff);
	}
	//Not needed: owp[words-1] |= 1<<VL_BITBIT_I(lbits-1);  // Set sign bit
    }
    // Last output word is dirty
    return(owp);
}

static inline IData VL_DIVS_III(int,int lbits,int, IData lhs,IData rhs) {
    if (rhs==0) return 0;
    vlsint32_t lhs_signed = VL_EXTENDS_II(32, lbits, lhs);
    vlsint32_t rhs_signed = VL_EXTENDS_II(32, lbits, rhs);
    return lhs_signed / rhs_signed;
}
static inline QData VL_DIVS_QQQ(int,int lbits,int, QData lhs,QData rhs) {
    if (rhs==0) return 0;
    vlsint64_t lhs_signed = VL_EXTENDS_QQ(64, lbits, lhs);
    vlsint64_t rhs_signed = VL_EXTENDS_QQ(64, lbits, rhs);
    return lhs_signed / rhs_signed;
}
static inline IData VL_MODDIVS_III(int,int lbits,int, IData lhs,IData rhs) {
    if (rhs==0) return 0;
    vlsint32_t lhs_signed = VL_EXTENDS_II(32, lbits, lhs);
    vlsint32_t rhs_signed = VL_EXTENDS_II(32, lbits, rhs);
    return lhs_signed % rhs_signed;
}
static inline QData VL_MODDIVS_QQQ(int,int lbits,int, QData lhs,QData rhs) {
    if (rhs==0) return 0;
    vlsint64_t lhs_signed = VL_EXTENDS_QQ(64, lbits, lhs);
    vlsint64_t rhs_signed = VL_EXTENDS_QQ(64, lbits, rhs);
    return lhs_signed % rhs_signed;
}

static inline IData VL_POW_III(int, int, int rbits, IData lhs, IData rhs) {
    if (lhs==0) return 0;
    IData power = lhs;
    IData out = 1;
    for (int i=0; i<rbits; i++) {
	if (i>0) power = power*power;
	if (rhs & (VL_ULL(1)<<i)) out *= power;
    }
    return out;
}

#define VL_POW_QQI(obits,lbits,rbits,lhs,rhs) VL_POW_QQQ(obits,lbits,rbits,lhs,rhs) 

static inline QData VL_POW_QQQ(int, int, int rbits, QData lhs, QData rhs) {
    if (lhs==0) return 0;
    QData power = lhs;
    QData out = VL_ULL(1);
    for (int i=0; i<rbits; i++) {
	if (i>0) power = power*power;
	if (rhs & (VL_ULL(1)<<i)) out *= power;
    }
    return out;
}

//===================================================================
// Concat/replication

// INTERNAL: Stuff LHS bit 0++ into OUTPUT at specified offset
// ld may be "dirty", output is clean
static inline void _VL_INSERT_II(int, CData& lhsr, IData ld, int hbit, int lbit) { 
    IData insmask = (VL_MASK_I(hbit-lbit+1))<<lbit;
    lhsr = (lhsr & ~insmask) | ((ld<<lbit) & insmask);
}
static inline void _VL_INSERT_II(int, SData& lhsr, IData ld, int hbit, int lbit) { 
    IData insmask = (VL_MASK_I(hbit-lbit+1))<<lbit;
    lhsr = (lhsr & ~insmask) | ((ld<<lbit) & insmask);
}
static inline void _VL_INSERT_II(int, IData& lhsr, IData ld, int hbit, int lbit) { 
    IData insmask = (VL_MASK_I(hbit-lbit+1))<<lbit;
    lhsr = (lhsr & ~insmask) | ((ld<<lbit) & insmask);
}
static inline void _VL_INSERT_QQ(int, QData& lhsr, QData ld, int hbit, int lbit) { 
    QData insmask = (VL_MASK_Q(hbit-lbit+1))<<lbit;
    lhsr = (lhsr & ~insmask) | ((ld<<lbit) & insmask);
}
static inline void _VL_INSERT_WI(int, WDataOutP owp, IData ld, int hbit, int lbit) { 
    int hoffset = VL_BITBIT_I(hbit);
    int loffset = VL_BITBIT_I(lbit);
    if (hoffset==VL_SIZEBITS_I && loffset==0) {
	// Fast and common case, word based insertion
	owp[VL_BITWORD_I(lbit)] = ld;
    }
    else {
	int hword = VL_BITWORD_I(hbit);
	int lword = VL_BITWORD_I(lbit);
	if (hword==lword) {	// know < 32 bits because above checks it
	    IData insmask = (VL_MASK_I(hoffset-loffset+1))<<loffset;
	    owp[lword] = (owp[lword] & ~insmask) | ((ld<<loffset) & insmask);
	} else {
	    IData hinsmask = (VL_MASK_I(hoffset-0+1))<<0;
	    IData linsmask = (VL_MASK_I(31-loffset+1))<<loffset;
	    int nbitsonright = 32-loffset;  // bits that end up in lword
	    owp[lword] = (owp[lword] & ~linsmask) | ((ld<<loffset) & linsmask);
	    owp[hword] = (owp[hword] & ~hinsmask) | ((ld>>nbitsonright) & hinsmask);
	}
    }
}

// INTERNAL: Stuff large LHS bit 0++ into OUTPUT at specified offset
// lwp may be "dirty"
static inline void _VL_INSERT_WW(int, WDataOutP owp, WDataInP lwp, int hbit, int lbit) { 
    int hoffset = hbit & VL_SIZEBITS_I;
    int loffset = lbit & VL_SIZEBITS_I;
    int lword = VL_BITWORD_I(lbit);
    int words = VL_WORDS_I(hbit-lbit+1);
    if (hoffset==VL_SIZEBITS_I && loffset==0) {
	// Fast and common case, word based insertion
	for (int i=0; i<words; i++) {
	    owp[lword+i] = lwp[i];
	}
    }
    else if (loffset==0) {
	// Non-32bit, but nicely aligned, so stuff all but the last word
	for (int i=0; i<(words-1); i++) {
	    owp[lword+i] = lwp[i];
	}
	IData hinsmask = (VL_MASK_I(hoffset-0+1)); // Know it's not a full word as above fast case handled it
	owp[lword+words-1] = (owp[words+lword-1] & ~hinsmask) | (lwp[words-1] & hinsmask);
    }
    else {
	IData hinsmask = (VL_MASK_I(hoffset-0+1))<<0;
	IData linsmask = (VL_MASK_I(31-loffset+1))<<loffset;
	int nbitsonright = 32-loffset;  // bits that end up in lword (know loffset!=0)
	// Middle words
	int hword = VL_BITWORD_I(hbit);
	for (int i=0; i<words; i++) {
	    {	// Lower word
		int oword = lword+i;
		IData d = lwp[i]<<loffset;
		IData od = owp[oword] & ~linsmask | d & linsmask;
		if (oword==hword) owp[oword] = owp[oword] & ~hinsmask | od & hinsmask;
		else owp[oword] = od;
	    }
	    {   // Upper word
		int oword = lword+i+1;
		if (oword <= hword) {
		    IData d = lwp[i]>>nbitsonright;
		    IData od = d & ~linsmask | owp[oword] & linsmask;
		    if (oword==hword) owp[oword] = owp[oword] & ~hinsmask | od & hinsmask;
		    else owp[oword] = od;
		}
	    }
	}
    }
}

static inline void _VL_INSERT_WQ(int obits, WDataOutP owp, QData ld, int hbit, int lbit) { 
    WData lwp[2]; VL_SET_WQ(lwp,ld);
    _VL_INSERT_WW(obits,owp,lwp,hbit,lbit);
}

// EMIT_RULE: VL_REPLICATE:  oclean=clean>width32, dirty<=width32; lclean=clean; rclean==clean;
// RHS MUST BE CLEAN CONSTANT.
#define VL_REPLICATE_IOI(obits,lbits,rbits, ld, rep) (-(ld))  // Iff lbits==1
#define VL_REPLICATE_QOI(obits,lbits,rbits, ld, rep) (-((QData)ld))  // Iff lbits==1

static inline IData VL_REPLICATE_III(int, int lbits, int, IData ld, IData rep) {
    IData returndata = ld;
    for (unsigned i=1; i < rep; i++){
	returndata = returndata << lbits;
	returndata |= ld;
    }
    return (returndata);
}
static inline QData VL_REPLICATE_QII(int, int lbits, int, IData ld, IData rep) {
    QData returndata = ld;
    for (unsigned i=1; i < rep; i++){
	returndata = returndata << lbits;
	returndata |= (QData)ld;
    }
    return (returndata);
}
static inline WDataOutP VL_REPLICATE_WII(int obits, int lbits, int, WDataOutP owp, IData ld, IData rep) {
    owp[0] = ld;
    for (unsigned i=1; i < rep; i++){
	_VL_INSERT_WI(obits,owp,ld,i*lbits+lbits-1,i*lbits);
    }
    return(owp);
}
static inline WDataOutP VL_REPLICATE_WQI(int obits, int lbits, int, WDataOutP owp, QData ld, IData rep) {
    VL_SET_WQ(owp,ld);
    for (unsigned i=1; i < rep; i++){
	_VL_INSERT_WQ(obits,owp,ld,i*lbits+lbits-1,i*lbits);
    }
    return(owp);
}
static inline WDataOutP VL_REPLICATE_WWI(int obits, int lbits, int, WDataOutP owp, WDataInP lwp, IData rep) {
    for (int i=0; i < VL_WORDS_I(lbits); i++) owp[i] = lwp[i];
    for (unsigned i=1; i < rep; i++){
	_VL_INSERT_WW(obits,owp,lwp,i*lbits+lbits-1,i*lbits);
    }
    return(owp);
}

// Because concats are common and wide, it's valuable to always have a clean output.
// Thus we specify inputs must be clean, so we don't need to clean the output.
// Note the bit shifts are always constants, so the adds in these constify out.
// Casts required, as args may be 8 bit entities, and need to shift to appropriate output size
#define VL_CONCAT_III(obits,lbits,rbits,ld,rd)   ((IData)(ld)<<(rbits) | (IData)(rd))
#define VL_CONCAT_QII(obits,lbits,rbits,ld,rd)   ((QData)(ld)<<(rbits) | (QData)(rd))
#define VL_CONCAT_QIQ(obits,lbits,rbits,ld,rd)   ((QData)(ld)<<(rbits) | (QData)(rd))
#define VL_CONCAT_QQI(obits,lbits,rbits,ld,rd)   ((QData)(ld)<<(rbits) | (QData)(rd))
#define VL_CONCAT_QQQ(obits,lbits,rbits,ld,rd)   ((QData)(ld)<<(rbits) | (QData)(rd))

static inline WDataOutP VL_CONCAT_WII(int obits,int lbits,int rbits,WDataOutP owp,IData ld,IData rd) {
    owp[0] = rd;
    for (int i=1; i < VL_WORDS_I(obits); i++) owp[i] = 0;
    _VL_INSERT_WI(obits,owp,ld,rbits+lbits-1,rbits);
    return(owp);
}
static inline WDataOutP VL_CONCAT_WWI(int obits,int lbits,int rbits,WDataOutP owp,WDataInP lwp, IData rd) {
    owp[0] = rd;
    for (int i=1; i < VL_WORDS_I(obits); i++) owp[i] = 0;
    _VL_INSERT_WW(obits,owp,lwp,rbits+lbits-1,rbits);
    return(owp);
}
static inline WDataOutP VL_CONCAT_WIW(int obits,int lbits,int rbits,WDataOutP owp,IData ld, WDataInP rwp) {
    for (int i=0; i < VL_WORDS_I(rbits); i++) owp[i] = rwp[i];
    for (int i=VL_WORDS_I(rbits); i < VL_WORDS_I(obits); i++) owp[i] = 0;
    _VL_INSERT_WI(obits,owp,ld,rbits+lbits-1,rbits);
    return(owp);
}
static inline WDataOutP VL_CONCAT_WIQ(int obits,int lbits,int rbits,WDataOutP owp,IData ld,QData rd) {
    VL_SET_WQ(owp,rd);
    for (int i=2; i < VL_WORDS_I(obits); i++) owp[i] = 0;
    _VL_INSERT_WI(obits,owp,ld,rbits+lbits-1,rbits);
    return(owp);
}
static inline WDataOutP VL_CONCAT_WQI(int obits,int lbits,int rbits,WDataOutP owp,QData ld,IData rd) {
    owp[0] = rd;
    for (int i=1; i < VL_WORDS_I(obits); i++) owp[i] = 0;
    _VL_INSERT_WQ(obits,owp,ld,rbits+lbits-1,rbits);
    return(owp);
}
static inline WDataOutP VL_CONCAT_WQQ(int obits,int lbits,int rbits,WDataOutP owp,QData ld,QData rd) {
    VL_SET_WQ(owp,rd);
    for (int i=2; i < VL_WORDS_I(obits); i++) owp[i] = 0;
    _VL_INSERT_WQ(obits,owp,ld,rbits+lbits-1,rbits);
    return(owp);
}
static inline WDataOutP VL_CONCAT_WWQ(int obits,int lbits,int rbits,WDataOutP owp,WDataInP lwp, QData rd) {
    VL_SET_WQ(owp,rd);
    for (int i=2; i < VL_WORDS_I(obits); i++) owp[i] = 0;
    _VL_INSERT_WW(obits,owp,lwp,rbits+lbits-1,rbits);
    return(owp);
}
static inline WDataOutP VL_CONCAT_WQW(int obits,int lbits,int rbits,WDataOutP owp,QData ld, WDataInP rwp) {
    for (int i=0; i < VL_WORDS_I(rbits); i++) owp[i] = rwp[i];
    for (int i=VL_WORDS_I(rbits); i < VL_WORDS_I(obits); i++) owp[i] = 0;
    _VL_INSERT_WQ(obits,owp,ld,rbits+lbits-1,rbits);
    return(owp);
}
static inline WDataOutP VL_CONCAT_WWW(int obits,int lbits,int rbits,WDataOutP owp,WDataInP lwp, WDataInP rwp) {
    for (int i=0; i < VL_WORDS_I(rbits); i++) owp[i] = rwp[i];
    for (int i=VL_WORDS_I(rbits); i < VL_WORDS_I(obits); i++) owp[i] = 0;
    _VL_INSERT_WW(obits,owp,lwp,rbits+lbits-1,rbits);
    return(owp);
}

//===================================================================
// Shifts

// EMIT_RULE: VL_SHIFTL:  oclean=lclean; rclean==clean;
// Important: Unlike most other funcs, the shift might well be a computed
// expression.  Thus consider this when optimizing.  (And perhaps have 2 funcs?)
static inline WDataOutP VL_SHIFTL_WWI(int obits,int,int,WDataOutP owp,WDataInP lwp, IData rd) {
    int word_shift = VL_BITWORD_I(rd);
    int bit_shift = VL_BITBIT_I(rd);
    if ((int)rd >= obits) {
	for (int i=0; i < VL_WORDS_I(obits); i++) owp[i] = 0;
    } else if (bit_shift==0) {  // Aligned word shift (<<0,<<32,<<64 etc)
	for (int i=0; i < word_shift; i++) owp[i] = 0;
	for (int i=word_shift; i < VL_WORDS_I(obits); i++) owp[i] = lwp[i-word_shift];
    } else {
	for (int i=0; i < VL_WORDS_I(obits); i++) owp[i] = 0;
	_VL_INSERT_WW(obits,owp,lwp,obits-1,rd);
    }
    return(owp);
}

// EMIT_RULE: VL_SHIFTR:  oclean=lclean; rclean==clean;
// Important: Unlike most other funcs, the shift might well be a computed
// expression.  Thus consider this when optimizing.  (And perhaps have 2 funcs?)
static inline WDataOutP VL_SHIFTR_WWI(int obits,int,int,WDataOutP owp,WDataInP lwp, IData rd) {
    int word_shift = VL_BITWORD_I(rd);  // Maybe 0
    int bit_shift = VL_BITBIT_I(rd);
    if ((int)rd >= obits) {
	for (int i=0; i < VL_WORDS_I(obits); i++) owp[i] = 0;
    } else if (bit_shift==0) {  // Aligned word shift (>>0,>>32,>>64 etc)
	int copy_words = (VL_WORDS_I(obits)-word_shift);
	for (int i=0; i < copy_words; i++) owp[i] = lwp[i+word_shift];
	for (int i=copy_words; i < VL_WORDS_I(obits); i++) owp[i] = 0;
    } else {
	int loffset = rd & VL_SIZEBITS_I;
	int nbitsonright = 32-loffset;  // bits that end up in lword (know loffset!=0)
	// Middle words
	int words = VL_WORDS_I(obits-rd);
	for (int i=0; i<words; i++) {
	    owp[i] = lwp[i+word_shift]>>loffset;
	    int upperword = i+word_shift+1;
	    if (upperword < VL_WORDS_I(obits)) {
		owp[i] |= lwp[upperword]<< nbitsonright;
	    }
	}
	for (int i=words; i<VL_WORDS_I(obits); i++) owp[i]=0;
    }
    return(owp);
}

// EMIT_RULE: VL_SHIFTRS:  oclean=false; lclean=clean, rclean==clean;
static inline IData VL_SHIFTRS_III(int obits, int, int, IData lhs, IData rhs) {
    // Note the C standard does not specify the >> operator as a arithmetic shift!
    IData sign    = -(lhs >> (obits-1)); // ffff_ffff if negative
    IData signext = ~(VL_MASK_I(obits) >> rhs);   // One with bits where we've shifted "past"
    return (lhs >> rhs) | sign & VL_CLEAN_II(obits,obits,signext);
}
static inline QData VL_SHIFTRS_QQI(int obits, int, int, QData lhs, IData rhs) {
    QData sign    = -(lhs >> (obits-1));
    QData signext = ~(VL_MASK_Q(obits) >> rhs);
    return (lhs >> rhs) | sign & VL_CLEAN_QQ(obits,obits,signext);
}
static inline WDataOutP VL_SHIFTRS_WWI(int obits,int,int,WDataOutP owp,WDataInP lwp, IData rd) {
    int word_shift = VL_BITWORD_I(rd);
    int bit_shift = VL_BITBIT_I(rd);
    int lmsw = VL_WORDS_I(obits)-1;
    IData sign = VL_SIGNONES_I(obits,lwp[lmsw]);
    if ((int)rd >= obits) {
	for (int i=0; i <= lmsw; i++) owp[i] = sign;
    } else if (bit_shift==0) {  // Aligned word shift (>>0,>>32,>>64 etc)
	int copy_words = (VL_WORDS_I(obits)-word_shift);
	for (int i=0; i < copy_words; i++) owp[i] = lwp[i+word_shift];
	if (copy_words>=0) owp[copy_words-1] |= ~VL_MASK_I(obits) & sign;
	for (int i=copy_words; i < VL_WORDS_I(obits); i++) owp[i] = sign;
    } else {
	int loffset = rd & VL_SIZEBITS_I;
	int nbitsonright = 32-loffset;  // bits that end up in lword (know loffset!=0)
	// Middle words
	int words = VL_WORDS_I(obits-rd);
	for (int i=0; i<words; i++) {
	    owp[i] = lwp[i+word_shift]>>loffset;
	    int upperword = i+word_shift+1;
	    if (upperword < VL_WORDS_I(obits)) {
		owp[i] |= lwp[upperword]<< nbitsonright;
	    }
	}
	if (words) owp[words-1] |= sign & ~VL_MASK_I(obits-loffset);
	for (int i=words; i<VL_WORDS_I(obits); i++) owp[i] = sign;
    }
    return(owp);
}

//===================================================================
// Bit selection

// EMIT_RULE: VL_BITSEL:  oclean=dirty; rclean==clean;
#define VL_BITSEL_IIII(obits,lbits,rbits,zbits,lhs,rhs) ((lhs)>>(rhs))
#define VL_BITSEL_QIII(obits,lbits,rbits,zbits,lhs,rhs) ((lhs)>>(rhs))
#define VL_BITSEL_QQII(obits,lbits,rbits,zbits,lhs,rhs) ((lhs)>>(rhs))
#define VL_BITSEL_IQII(obits,lbits,rbits,zbits,lhs,rhs) ((IData)((lhs)>>(rhs)))

static inline IData VL_BITSEL_IWII(int, int lbits, int, int, WDataInP lwp, IData rd) {
    int word = VL_BITWORD_I(rd);
    if ((int)rd>lbits) {
	return ~0; // Spec says you can go outside the range of a array.  Don't coredump if so.
	// We return all 1's as that's more likely to find bugs (?) then 0's.
    } else {
	return (lwp[word]>>VL_BITBIT_I(rd));
    }
}

// EMIT_RULE: VL_RANGE:  oclean=lclean;  out=dirty
// <msb> & <lsb> MUST BE CLEAN (currently constant)
#define VL_SEL_IIII(obits,lbits,rbits,tbits,lhs,lsb,width) ((lhs)>>(lsb))
#define VL_SEL_QQII(obits,lbits,rbits,tbits,lhs,lsb,width) ((lhs)>>(lsb))
#define VL_SEL_IQII(obits,lbits,rbits,tbits,lhs,lsb,width) ((IData)((lhs)>>(lsb)))

static inline IData VL_SEL_IWII(int, int lbits, int, int, WDataInP lwp, IData lsb, IData width) {
    int msb = lsb+width-1;
    if (msb>lbits) {
	return ~0; // Spec says you can go outside the range of a array.  Don't coredump if so.
    } else if (VL_BITWORD_I(msb)==VL_BITWORD_I((int)lsb)) {
	return (lwp[VL_BITWORD_I(lsb)]>>VL_BITBIT_I(lsb));
    } else {
	// 32 bit extraction may span two words
	int nbitsfromlow = 32-VL_BITBIT_I(lsb);  // bits that come from low word
	return ((lwp[VL_BITWORD_I(msb)]<<nbitsfromlow)
		|(lwp[VL_BITWORD_I(lsb)]>>VL_BITBIT_I(lsb)));
    }
}

static inline QData VL_SEL_QWII(int, int lbits, int, int, WDataInP lwp, IData lsb, IData width) {
    int msb = lsb+width-1;
    if (msb>lbits) {
	return ~0; // Spec says you can go outside the range of a array.  Don't coredump if so.
    } else if (VL_BITWORD_I(msb)==VL_BITWORD_I((int)lsb)) {
	return (lwp[VL_BITWORD_I(lsb)]>>VL_BITBIT_I(lsb));
    } else if (VL_BITWORD_I(msb)==1+VL_BITWORD_I((int)lsb)) {
	int nbitsfromlow = 32-VL_BITBIT_I(lsb);
	QData hi = (lwp[VL_BITWORD_I(msb)]);
	QData lo = (lwp[VL_BITWORD_I(lsb)]>>VL_BITBIT_I(lsb));
	return (hi<<nbitsfromlow) | lo;
    } else {
	// 64 bit extraction may span three words
	int nbitsfromlow = 32-VL_BITBIT_I(lsb);
	QData hi = (lwp[VL_BITWORD_I(msb)]);
	QData mid= (lwp[VL_BITWORD_I(lsb)+1]);
	QData lo = (lwp[VL_BITWORD_I(lsb)]>>VL_BITBIT_I(lsb));
	return (hi<<(nbitsfromlow+32)) | (mid<<nbitsfromlow) | lo;
    }
}

static inline WDataOutP VL_SEL_WWII(int obits,int lbits,int,int,WDataOutP owp,WDataInP lwp, IData lsb, IData width) {
    int msb = lsb+width-1;
    int word_shift = VL_BITWORD_I(lsb);
    if (msb>lbits) { // Outside bounds, 
	for (int i=0; i<VL_WORDS_I(obits)-1; i++) owp[i] = ~0;
	owp[VL_WORDS_I(obits)-1] = VL_MASK_I(obits);
    } else if (VL_BITBIT_I(lsb)==0) {
	// Just a word extract
	for (int i=0; i<VL_WORDS_I(obits); i++) owp[i] = lwp[i+word_shift];
    } else {
	// Not a _VL_INSERT because the bits come from any bit number and goto bit 0
	int loffset = lsb & VL_SIZEBITS_I;
	int nbitsfromlow = 32-loffset;  // bits that end up in lword (know loffset!=0)
	// Middle words
	int words = VL_WORDS_I(msb-lsb+1);
	for (int i=0; i<words; i++) {
	    owp[i] = lwp[i+word_shift]>>loffset;
	    int upperword = i+word_shift+1;
	    if (upperword <= (int)VL_BITWORD_I(msb)) {
		owp[i] |= lwp[upperword]<< nbitsfromlow;
	    }
	}
	for (int i=words; i<VL_WORDS_I(obits); i++) owp[i]=0;
    }
    return owp;
}

//======================================================================
// Range assignments

// EMIT_RULE: VL_ASSIGNRANGE:  rclean=dirty;
static inline void VL_ASSIGNSEL_IIII(int obits, int lsb, CData& lhsr, IData rhs) {
    _VL_INSERT_II(obits, lhsr, rhs, lsb+obits-1, lsb);
}
static inline void VL_ASSIGNSEL_IIII(int obits, int lsb, SData& lhsr, IData rhs) {
    _VL_INSERT_II(obits, lhsr, rhs, lsb+obits-1, lsb);
}
static inline void VL_ASSIGNSEL_IIII(int obits, int lsb, IData& lhsr, IData rhs) {
    _VL_INSERT_II(obits, lhsr, rhs, lsb+obits-1, lsb);
}
static inline void VL_ASSIGNSEL_QIII(int obits, int lsb, QData& lhsr, IData rhs) {
    _VL_INSERT_QQ(obits, lhsr, rhs, lsb+obits-1, lsb);
}
static inline void VL_ASSIGNSEL_QQII(int obits, int lsb, QData& lhsr, QData rhs) {
    _VL_INSERT_QQ(obits, lhsr, rhs, lsb+obits-1, lsb);
}
static inline void VL_ASSIGNSEL_QIIQ(int obits, int lsb, QData& lhsr, QData rhs) {
    _VL_INSERT_QQ(obits, lhsr, rhs, lsb+obits-1, lsb);
}
//static inline void VL_ASSIGNSEL_IIIW(int obits, int lsb, IData& lhsr,WDataInP rwp) {
// Illegal, as lhs width >= rhs width
static inline void VL_ASSIGNSEL_WIII(int obits, int lsb, WDataOutP owp, IData rhs) {
    _VL_INSERT_WI(obits, owp, rhs, lsb+obits-1, lsb);
}
static inline void VL_ASSIGNSEL_WIIQ(int obits, int lsb, WDataOutP owp, QData rhs) {
    _VL_INSERT_WQ(obits, owp, rhs, lsb+obits-1, lsb);
}
static inline void VL_ASSIGNSEL_WIIW(int obits, int lsb, WDataOutP owp, WDataInP rwp) {
    _VL_INSERT_WW(obits, owp, rwp, lsb+obits-1, lsb);
}

//======================================================================
// Triops

static inline WDataOutP VL_COND_WIWW(int obits, int, int, int,
				     WDataOutP owp, int cond, WDataInP w1p, WDataInP w2p) {
    int words = VL_WORDS_I(obits);
    for (int i=0; i < words; i++) owp[i] = cond ? w1p[i] : w2p[i];
    return(owp);
}

//======================================================================
// Constification

// VL_CONST_W_#X(int obits, WDataOutP owp, IData data0, .... IData data(#-1))
// Sets wide vector words to specified constant words, zeros upper data.

#define I IData
#define _END(obits,wordsSet) \
    for(int i=(wordsSet);i<VL_WORDS_I(obits);i++) o[i] = (IData)0x0; \
    return o

static inline WDataOutP VL_CONST_W_1X(int obits, WDataOutP o,
				      I d0) {
    o[0]=d0;
    _END(obits,1);  }
static inline WDataOutP VL_CONST_W_2X(int obits, WDataOutP o,
				      I d1,I d0) {
    o[0]=d0;  o[1]=d1;
    _END(obits,2);  }
static inline WDataOutP VL_CONST_W_3X(int obits, WDataOutP o,
				      I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;
    _END(obits,3);  }
static inline WDataOutP VL_CONST_W_4X(int obits, WDataOutP o, 
				      I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;
    _END(obits,4);  }
static inline WDataOutP VL_CONST_W_5X(int obits, WDataOutP o, 
				      I d4,I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;  o[4]=d4;
    _END(obits,5);  }
static inline WDataOutP VL_CONST_W_6X(int obits, WDataOutP o, 
				      I d5,I d4,I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;  o[4]=d4;  o[5]=d5;
    _END(obits,6);  }
static inline WDataOutP VL_CONST_W_7X(int obits, WDataOutP o, 
				      I d6,I d5,I d4,I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;  o[4]=d4;  o[5]=d5;  o[6]=d6;
    _END(obits,7);  }
static inline WDataOutP VL_CONST_W_8X(int obits, WDataOutP o, 
				      I d7,I d6,I d5,I d4,I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;  o[4]=d4;  o[5]=d5;  o[6]=d6;  o[7]=d7;
    _END(obits,8);  }
static inline WDataOutP VL_CONST_W_9X(int obits, WDataOutP o, 
				      I d8,
				      I d7,I d6,I d5,I d4,I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;  o[4]=d4;  o[5]=d5;  o[6]=d6;  o[7]=d7;
    o[8]=d8;
    _END(obits,9);  }
static inline WDataOutP VL_CONST_W_10X(int obits, WDataOutP o, 
				       I d9,I d8,
				       I d7,I d6,I d5,I d4,I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;  o[4]=d4;  o[5]=d5;  o[6]=d6;  o[7]=d7;
    o[8]=d8;  o[9]=d9;
    _END(obits,10);  }
static inline WDataOutP VL_CONST_W_11X(int obits, WDataOutP o, 
				       I d10,I d9,I d8,
				       I d7,I d6,I d5,I d4,I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;  o[4]=d4;  o[5]=d5;  o[6]=d6;  o[7]=d7;
    o[8]=d8;  o[9]=d9;  o[10]=d10;
    _END(obits,11);  }
static inline WDataOutP VL_CONST_W_12X(int obits, WDataOutP o, 
				       I d11,I d10,I d9,I d8,
				       I d7,I d6,I d5,I d4,I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;  o[4]=d4;  o[5]=d5;  o[6]=d6;  o[7]=d7;
    o[8]=d8;  o[9]=d9;  o[10]=d10; o[11]=d11;
    _END(obits,12);  }
static inline WDataOutP VL_CONST_W_16X(int obits, WDataOutP o, 
				       I d15,I d14,I d13,I d12,I d11,I d10,I d9,I d8,
				       I d7,I d6,I d5,I d4,I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;  o[4]=d4;  o[5]=d5;  o[6]=d6;  o[7]=d7;
    o[8]=d8;  o[9]=d9;  o[10]=d10;  o[11]=d11;  o[12]=d12;  o[13]=d13;  o[14]=d14;  o[15]=d15;
    _END(obits,16);  }
static inline WDataOutP VL_CONST_W_24X(int obits, WDataOutP o, 
				       I d23,I d22,I d21,I d20,I d19,I d18,I d17,I d16,
				       I d15,I d14,I d13,I d12,I d11,I d10,I d9,I d8,
				       I d7,I d6,I d5,I d4,I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;  o[4]=d4;  o[5]=d5;  o[6]=d6;  o[7]=d7;
    o[8]=d8;  o[9]=d9;  o[10]=d10;  o[11]=d11;  o[12]=d12;  o[13]=d13;  o[14]=d14;  o[15]=d15;
    o[16]=d16;  o[17]=d17;  o[18]=d18;  o[19]=d19;  o[20]=d20;  o[21]=d21;  o[22]=d22;  o[23]=d23;
    _END(obits,24);  }
static inline WDataOutP VL_CONST_W_28X(int obits, WDataOutP o, 
				       I d27,I d26,I d25,I d24,
				       I d23,I d22,I d21,I d20,I d19,I d18,I d17,I d16,
				       I d15,I d14,I d13,I d12,I d11,I d10,I d9,I d8,
				       I d7,I d6,I d5,I d4,I d3,I d2,I d1,I d0) {
    o[0]=d0;  o[1]=d1;  o[2]=d2;  o[3]=d3;  o[4]=d4;  o[5]=d5;  o[6]=d6;  o[7]=d7;
    o[8]=d8;  o[9]=d9;  o[10]=d10;  o[11]=d11;  o[12]=d12;  o[13]=d13;  o[14]=d14;  o[15]=d15;
    o[16]=d16;  o[17]=d17;  o[18]=d18;  o[19]=d19;  o[20]=d20;  o[21]=d21;  o[22]=d22;  o[23]=d23;
    o[24]=d24;  o[25]=d25;  o[26]=d26;  o[27]=d27;
    _END(obits,28);  }

#undef _END
#undef I

// Debugging

#endif /*_VERILATED_H_*/
