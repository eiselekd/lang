// $Id$ //-*- C++ -*-
//*************************************************************************
// DESCRIPTION: Verilator: Error handling
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

#ifndef _V3ERROR_H_
#define _V3ERROR_H_ 1
#include "config.h"
#include <string>
#include <iostream>
#include <sstream>
#include <bitset>

//######################################################################

class V3ErrorCode {
public:
    enum en {
	SUPPRESS,	// Warning suppressed by user
	FATAL,		// Kill the program
	ERROR,		// Error out, can't suppress
	// Warning codes:
	FIRST_WARN,	// Just a code so the program knows where to start warnings
	BLKANDNBLK,	// Blocked and non-blocking assignments to same variable
	CASEINCOMPLETE,	// Case statement has missing values
	CASEOVERLAP,	// Case statements overlap
	CASEX,		// Casex
	CMPCONST,	// Comparison is constant due to limited range
	COMBDLY,	// Combinatorial delayed assignment
	GENCLK,		// Generated Clock
	IMPLICIT,	// Implicit wire
	MULTIDRIVEN,	// Driven from multiple blocks
	UNDRIVEN,	// No drivers
	UNOPT,		// Unoptimizable block
	UNOPTFLAT,	// Unoptimizable block after flattening
	UNSIGNED,	// Comparison is constant due to unsigned arithmetic
	UNUSED,		// No receivers
	VARHIDDEN,	// Hiding variable
	WIDTH,		// Width mismatch
	MAX
	// ***Add new elements below also***
    };
    enum en m_e;
    inline V3ErrorCode () {};
    inline V3ErrorCode (en _e) : m_e(_e) {};
    V3ErrorCode (const char* msgp);	// Matching code or ERROR
    explicit inline V3ErrorCode (int _e) : m_e(static_cast<en>(_e)) {};
    operator en () const { return m_e; };
    const char* ascii() const {
	const char* names[] = {
	    // Leading spaces indicate it can't be disabled.
	    " SUPPRESS", " FATAL", " ERROR",
	    " FIRST_WARN",
	    "BLKANDNBLK",
	    "CASEINCOMPLETE", "CASEOVERLAP", "CASEX", "CMPCONST",
	    "COMBDLY", "GENCLK", "IMPLICIT",
	    "MULTIDRIVEN",
	    "UNDRIVEN", "UNOPT", "UNOPTFLAT", "UNSIGNED", "UNUSED",
	    "VARHIDDEN", "WIDTH",
	    " MAX"
	};
	return names[m_e];
    };
    // Warnings that warn about nasty side effects
    bool dangerous() const { return ( m_e==COMBDLY );};
  };
  inline bool operator== (V3ErrorCode lhs, V3ErrorCode rhs) { return (lhs.m_e == rhs.m_e); }
  inline bool operator== (V3ErrorCode lhs, V3ErrorCode::en rhs) { return (lhs.m_e == rhs); }
  inline bool operator== (V3ErrorCode::en lhs, V3ErrorCode rhs) { return (lhs == rhs.m_e); }
  inline ostream& operator<<(ostream& os, V3ErrorCode rhs) { return os<<rhs.ascii(); }

//######################################################################

class V3Error {
    // Base class for any object that wants debugging and error reporting
  private:
    static bool 	s_describedWarnings;	// Told user how to disable warns
    static bool 	s_describedEachWarn[V3ErrorCode::MAX]; // Told user specifics about this warning
    static bool 	s_pretendError[V3ErrorCode::MAX]; // Pretend this warning is an error
    static int		s_debugDefault;		// Default debugging level
    static int		s_errcnt;		// Error count
    static ostringstream s_errorStr;		// Error string being formed
    static V3ErrorCode	s_errorCode;		// Error string being formed will abort
    enum MaxErrors { 	MAX_ERRORS = 50 };	// Fatal after this may errors
    static void	incErrors();

    V3Error() { cerr<<("Static class"); abort(); }

  public:
    // CREATORS
    // ACCESSORS
    static void		debugDefault(int level) { s_debugDefault = level; }
    static int		debugDefault() { return s_debugDefault; }
    static string	msgPrefix(V3ErrorCode code=s_errorCode);	// returns %Error/%Warn
    static int		errorCount() { return s_errcnt; }
    // METHODS
    static void		init();
    static void		abortIfErrors();
    static void		suppressThisWarning();	// Suppress next %Warn if user has it off
    static void		pretendError(V3ErrorCode code, bool flag) { s_pretendError[code]=flag; }
    static string 	v3sform (const char* format, ...);
    static string	lineStr (const char* filename, int lineno);
    static V3ErrorCode	errorCode() { return s_errorCode; }

    // Internals for v3error()/v3fatal() macros only
    // Error end takes the string stream to output, be careful to seek() as needed
    static ostringstream& v3errorPrep (V3ErrorCode code) {
	s_errorStr.str(""); s_errorCode=code; return s_errorStr; }
    static ostringstream& v3errorStr () { return s_errorStr; }
    static void	v3abort();
    static void	v3errorEnd(ostringstream& sstr);	// static, but often overridden in classes.
};

// Global versions, so that if the class doesn't define a operator, we get the functions anyways.
inline int debug() { return V3Error::debugDefault(); }
inline void v3errorEnd(ostringstream& sstr) { V3Error::v3errorEnd(sstr); }

// These allow errors using << operators: v3error("foo"<<"bar");
// Careful, you can't put () around msg, as you would in most macro definitions
#define v3fatal(msg) v3errorEnd(((V3Error::v3errorPrep(V3ErrorCode::FATAL)<<msg),V3Error::v3errorStr()));
#define v3error(msg) v3errorEnd(((V3Error::v3errorPrep(V3ErrorCode::ERROR)<<msg),V3Error::v3errorStr()));
#define v3warn(code,msg) v3errorEnd(((V3Error::v3errorPrep(V3ErrorCode::code)<<msg),V3Error::v3errorStr()));
// Use this instead of fatal() to mention the source code line.
#define v3fatalSrc(msg) v3fatal("Internal Error: "<<__FILE__<<":"<<dec<<__LINE__<<": "<<msg)

#define UINFO(level,stmsg) {if(debug()>=(level)) { cout<<"- "<<V3Error::lineStr(__FILE__,__LINE__)<<stmsg; }}
#define UINFONL(level,stmsg) {if(debug()>=(level)) { cout<<stmsg; } }

#define UDEBUGONLY(stmts) {stmts}
#define UASSERT(condition,stmsg) { if (!(condition)) { v3fatalSrc(stmsg); }}
// For use in V3Ast static functions only
#define UASSERT_STATIC(condition,stmsg) { if (!(condition)) { cerr<<"Internal Error: "<<__FILE__<<":"<<dec<<__LINE__<<(stmsg)<<endl; abort(); } }

#define V3ERROR_NA v3error("Internal: Unexpected Call")

//----------------------------------------------------------------------

template< class T> std::string cvtToStr (const T& t) {
    ostringstream os; os<<t; return os.str();
}

//######################################################################

class FileLine {
    // File and line number of an object, mostly for error reporting
    int		m_lineno;
    string	m_filename;
    bitset<V3ErrorCode::MAX>	m_warnOff;
    static FileLine s_defaultFileLine;
    struct EmptySecret {};
protected:
    // User routines should never need to change line numbers
    // We are storing pointers, so we CAN'T change them after initial reading.
    friend class V3Read;
    friend class V3PreLex;
    void lineno(int num) { m_lineno = num; }
    void filename(const string& name) { m_filename = name; }
    void lineDirective(const char* textp);
    void incLineno() { m_lineno++; }
    FileLine* copyOrSameFileLine();
public:
    FileLine (const string& filename, int lineno) { m_lineno=lineno; m_filename = filename; m_warnOff=s_defaultFileLine.m_warnOff;}
    FileLine (FileLine* fromp) { m_lineno=fromp->lineno(); m_filename = fromp->filename(); m_warnOff=fromp->m_warnOff;};
    FileLine (EmptySecret) { m_lineno=0; m_filename="COMMAND_LINE"; m_warnOff=0; } // Only for static constructor
    static FileLine& defaultFileLine() { return s_defaultFileLine; }
    int lineno () const { return m_lineno; }
    string ascii() const;
    const string filename () const { return m_filename; }
    const string filebasename () const;
    const char* cfilename () const { return m_filename.c_str(); }
    const string profileFuncname() const;
    void warnOff(V3ErrorCode code, bool flag) { m_warnOff.set(code,flag); }	// Turn on/off warning messages on this line.
    bool warnOff(const string& code, bool flag);  // Returns 1 if ok
    bool warnIsOff(V3ErrorCode code);
    void warnResetDefault() { m_warnOff=s_defaultFileLine.m_warnOff; }

    void	v3errorEnd(ostringstream& str);
    inline bool operator==(FileLine rhs) { return (m_lineno==rhs.m_lineno && m_filename==rhs.m_filename); }
};
ostream& operator<<(ostream& os, FileLine* fileline);

#endif // Guard
