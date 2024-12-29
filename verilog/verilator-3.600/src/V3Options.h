// $Id$ //-*- C++ -*-
//*************************************************************************
// DESCRIPTION: Verilator: Command line options
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

#ifndef _V3OPTIONS_H_
#define _V3OPTIONS_H_ 1

#include "config.h"
#include <string>
#include <set>

#include "V3Global.h"

//######################################################################
// V3Options - Command line options

class V3OptionsImp;
class FileLine;

typedef set<string> V3StringSet;

class V3Options {
    // MEMBERS (general options)
    V3OptionsImp*	m_impp;		// Slow hidden options

    V3StringSet	m_cppFiles;	// C++ files to link against
    V3StringSet	m_libraryFiles;	// Verilog -v files

    bool	m_preprocOnly;	// main switch: -E
    bool	m_makeDepend;	// main switch: -MMD
    bool	m_makePhony;	// main switch: -MP
    bool	m_assert;	// main switch: --assert
    bool	m_coverageLine;	// main switch: --coverage-block
    bool	m_coverageUser;	// main switch: --coverage-func
    bool	m_dumpTree;	// main switch: --dump-tree
    bool	m_exe;		// main switch: --exe
    bool	m_ignc;		// main switch: --ignc
    bool	m_inhibitSim;	// main switch: --inhibit-sim
    bool	m_l2Name;	// main switch: --l2name
    bool	m_outFormatOk;	// main switch: --cc, --sc or --sp was specified
    bool	m_pins64;	// main switch: --pins64
    bool	m_profileCFuncs;// main switch: --profile-cfuncs
    bool	m_psl;		// main switch: --psl
    bool	m_public;	// main switch: --public
    bool	m_systemC;	// main switch: --sc: System C instead of simple C++
    bool	m_skipIdentical;// main switch: --skip-identical
    bool	m_systemPerl;	// main switch: --sp: System Perl instead of SystemC (m_systemC also set)
    bool	m_stats;	// main switch: --stats
    bool	m_trace;	// main switch: --trace
    bool	m_traceDups;	// main switch: --trace-dups
    bool	m_underlineZero;// main switch: --underline-zero

    int		m_inlineMult;	// main switch: --inline-mult
    int		m_outputSplit;	// main switch: --output-split
    int		m_unrollCount;	// main switch: --unroll-count
    int		m_unrollStmts;	// main switch: --unroll-stmts

    string	m_bin;		// main switch: --bin {binary}
    string	m_flags;	// main switch: -f {name}
    string	m_top;		// main switch: Top .v file name
    string	m_makeDir;	// main switch: -Mdir
    string	m_prefix;	// main switch: --prefix
    string	m_modPrefix;	// main switch: --mod-prefix
    string	m_xAssign;	// main switch: --x-assign

    // MEMBERS (optimizations)
    //				// main switch: -Op: --public
    bool	m_oAcycSimp;	// main switch: -Oy: acyclic pre-optimizations
    bool	m_oCase;	// main switch: -Oe: case tree conversion
    bool	m_oCombine;	// main switch: -Ob: common icode packing
    bool	m_oConst;	// main switch: -Oc: constant folding
    bool	m_oExpand;	// main switch: -Ox: expansion of C macros
    bool	m_oGate;	// main switch: -Og: gate wire elimination
    bool	m_oLife;	// main switch: -Ol: variable lifetime
    bool	m_oLifePost;	// main switch: -Ot: delayed assignment elimination
    bool	m_oLocalize;	// main switch: -Oz: convert temps to local variables
    bool	m_oInline;	// main switch: -Oi: module inlining
    bool	m_oReorder;	// main switch: -Or: reorder assignments in blocks
    bool	m_oSplit;	// main switch: -Os: always assignment splitting
    bool	m_oSubst;	// main switch: -Ou: substitute expression temp values
    bool	m_oSubstConst;	// main switch: -Ok: final constant substitution
    bool	m_oTable;	// main switch: -Oa: lookup table creation

  private:
    // METHODS
    void addArg(const string& incdir);
    void addIncDir(const string& incdir);
    void addLibExt(const string& libext);
    void addDefine(const string& defline);
    void optimize(int level);
    void coverage(bool flag) { m_coverageLine = m_coverageUser = flag; }
    bool onoff(const char* sw, const char* arg, bool& flag);

  public:
    // CREATORS
    V3Options();
    ~V3Options();
    void setDebugMode(int level);

    // METHODS
    void addCppFile(const string& filename);
    void addLibraryFile(const string& filename);

    // ACCESSORS (options)
    const string& top() const { return m_top; }
    bool preprocOnly() const { return m_preprocOnly; }
    bool makeDepend() const { return m_makeDepend; }
    bool makePhony() const { return m_makePhony; }
    bool underlineZero() const { return m_underlineZero; }
    string bin() const { return m_bin; }
    string flags() const { return m_flags; }
    bool systemC() const { return m_systemC; }
    bool systemPerl() const { return m_systemPerl; }
    bool skipIdentical() const { return m_skipIdentical; }
    bool stats() const { return m_stats; }
    bool assertOn() const { return m_assert; }  // assertOn as "assert" may be defined
    bool coverage() const { return m_coverageUser || m_coverageLine; }
    bool coverageLine() const { return m_coverageLine; }
    bool coverageUser() const { return m_coverageUser; }
    bool dumpTree() const { return m_dumpTree; }
    bool exe() const { return m_exe; }
    bool trace() const { return m_trace; }
    bool traceDups() const { return m_traceDups; }
    bool outFormatOk() const { return m_outFormatOk; }
    bool pins64() const { return m_pins64; }
    bool profileCFuncs() const { return m_profileCFuncs; }
    bool psl() const { return m_psl; }
    bool allPublic() const { return m_public; }
    bool l2Name() const { return m_l2Name; }
    bool ignc() const { return m_ignc; }
    bool inhibitSim() const { return m_inhibitSim; }

    int	   inlineMult() const { return m_inlineMult; }
    int	   outputSplit() const { return m_outputSplit; }
    int	   unrollCount() const { return m_unrollCount; }
    int	   unrollStmts() const { return m_unrollStmts; }

    string makeDir() const { return m_makeDir; }
    string prefix() const { return m_prefix; }
    string modPrefix() const { return m_modPrefix; }
    string xAssign() const { return m_xAssign; }
    const V3StringSet& cppFiles() const { return m_cppFiles; }
    const V3StringSet& libraryFiles() const { return m_libraryFiles; }

    // ACCESSORS (optimization options)
    bool oAcycSimp() const { return m_oAcycSimp; }
    bool oCase() const { return m_oCase; }
    bool oCombine() const { return m_oCombine; }
    bool oConst() const { return m_oConst; }
    bool oExpand() const { return m_oExpand; }
    bool oGate() const { return m_oGate; }
    bool oDup() const { return oLife(); }
    bool oLife() const { return m_oLife; }
    bool oLifePost() const { return m_oLifePost; }
    bool oLocalize() const { return m_oLocalize; }
    bool oInline() const { return m_oInline; }
    bool oReorder() const { return m_oReorder; }
    bool oSplit() const { return m_oSplit; }
    bool oSubst() const { return m_oSubst; }
    bool oSubstConst() const { return m_oSubstConst; }
    bool oTable() const { return m_oTable; }

    // METHODS (from main)
    static string version();
    static string argString(int argc, char** argv);	///< Return list of arguments as simple string
    string allArgsString();	///< Return all passed arguments as simple string
    void parseOpts(FileLine* fl, int argc, char** argv);
    void parseOptsList (FileLine* fl, int argc, char** argv);
    void parseOptsFile (FileLine* fl, const string& filename);

    // METHODS (generic file utilities)
    static string filenameFromDirBase (const string& dir, const string& basename);
    static string filenameNonDir (const string& filename);	///< Return non-directory part of filename
    static string filenameNonExt (const string& filename);	///< Return non-extensioned (no .) part of filename
    static string filenameNonDirExt (const string& filename) { return filenameNonExt(filenameNonDir(filename)); }	///< Return basename of filename
    static string filenameDir (const string& filename);	///< Return directory part of filename
    static string getenvStr(const char* envvar, const char* defaultValue) {
	if (const char* envvalue = getenv(envvar)) {
	    return envvalue;
	} else {
	    return defaultValue;
	}
    }

    // METHODS (file utilities using these options)
    string fileExists (const string& filename);
    string filePath (FileLine* fl, const string& modname, const string& errmsg);
    static bool fileStatNormal (const string& filename);
};

//######################################################################

#endif // guard
