// $Id$ -*- C++ -*-
//*************************************************************************
// DESCRIPTION: Verilator: Emit C++ for tree
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

#ifndef _V3EMITCBASE_H_
#define _V3EMITCBASE_H_ 1

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <math.h>

#include "V3Global.h"
#include "V3File.h"
#include "V3Ast.h"

//######################################################################
// V3OutCFile: A class for abstracting out SystemC/C++ details

class V3OutCFile : public V3OutFile {
    int		m_private;
public:
    V3OutCFile(const string& filename) : V3OutFile(filename) {
	resetPrivate();
    }
    virtual ~V3OutCFile() {};
    virtual void putsCellDecl(const string& classname, const string& cellname) {
	this->printf("%-19s\t%s;\n",
		     (classname + "*").c_str(),cellname.c_str());
    }
    virtual void putsHeader() { puts("// Verilated -*- C++ -*-\n"); }
    virtual void putsIntTopInclude() { }
    // Print out public/privates
    void resetPrivate() { m_private = 0; }
    void putsPrivate(bool setPrivate) {
	if (setPrivate && m_private!=1) {
	    puts("private:\n");
	    m_private = 1;
	} else if (!setPrivate && m_private!=2) {
	    puts("public:\n");
	    m_private = 2;
	}
    }
};

class V3OutScFile : public V3OutCFile {
public:
    V3OutScFile(const string& filename) : V3OutCFile(filename) {}
    virtual ~V3OutScFile() {};
    virtual void putsHeader() { puts("// Verilated -*- SystemC -*-\n"); }
    virtual void putsIntTopInclude() { puts("#include \"systemc.h\"\n"); }
};

class V3OutSpFile : public V3OutCFile {
public:
    V3OutSpFile(const string& filename) : V3OutCFile(filename) {}
    virtual ~V3OutSpFile() {};
    virtual void putsHeader() { puts("// Verilated -*- SystemC -*-\n"); }
    virtual void putsIntTopInclude() { puts("#include \"systemperl.h\"\n"); }
};

class V3OutVFile : public V3OutCFile {
public:
    V3OutVFile(const string& filename) : V3OutCFile(filename) {}
    virtual ~V3OutVFile() {};
    virtual void putsHeader() { puts("// Verilated -*- Verilog -*-\n"); }
};

class V3OutMkFile : public V3OutFile {
public:
    V3OutMkFile(const string& filename) : V3OutFile(filename) {}
    virtual ~V3OutMkFile() {};
    virtual void putsHeader() { puts("# Verilated -*- Makefile -*-\n"); }
    // No automatic indentation yet.
    void puts(const char* strg) { putsNoTracking(strg); }
    void puts(const string& strg) { putsNoTracking(strg); }
};

//######################################################################
// Base Visitor class -- holds output file pointer

class EmitCBaseVisitor : public AstNVisitor {
public:
    // STATE
    V3OutCFile*	m_ofp;
    // METHODS
    V3OutCFile*	ofp() const { return m_ofp; };
    void puts(const string& str) { ofp()->puts(str); }
    void putbs(const string& str) { ofp()->putbs(str); }
    bool optSystemC() { return v3Global.opt.systemC(); }
    bool optSystemPerl() { return v3Global.opt.systemPerl(); }
    static string symClassName() { return v3Global.opt.prefix()+"__Syms"; }
    static string symClassVar()  { return symClassName()+"* __restrict vlSymsp"; }
    static string symTopAssign() { return v3Global.opt.prefix()+"* __restrict vlTOPp = vlSymsp->TOPp;"; }
    static string modClassName(AstModule* modp) {	// Return name of current module being processed
	if (modp->isTop()) {
	    return v3Global.opt.prefix();
	} else {
	    return v3Global.opt.modPrefix() + "_" + modp->name();
	}
    }
    static string topClassName() {		// Return name of top wrapper module
	return v3Global.opt.prefix();
    }
    AstCFile* newCFile(const string& filename, bool slow, bool source) {
	AstCFile* cfilep = new AstCFile(v3Global.rootp()->fileline(), filename);
	cfilep->slow(slow);
	cfilep->source(source);
	v3Global.rootp()->addFilesp(cfilep);
	return cfilep;
    }

    // CONSTRUCTORS
    EmitCBaseVisitor() {
	m_ofp = NULL;
    }
    virtual ~EmitCBaseVisitor() {}
};

//######################################################################
// Fileize state, as a visitor of each AstNode

class EmitCBaseCounterVisitor : public AstNVisitor {
private:
    // STATE
    int			m_count;	// Number of statements
    // VISITORS
    virtual void visit(AstNode* nodep, AstNUser*) {
	m_count++;
	nodep->iterateChildren(*this);
    }
public:
    // CONSTUCTORS
    EmitCBaseCounterVisitor(AstNode* nodep) {
	m_count = 0;
	nodep->accept(*this);
    }
    virtual ~EmitCBaseCounterVisitor() {}
    int count() const { return m_count; }
};

#endif // guard
