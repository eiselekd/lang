// $Id$
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

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "V3Error.h"
#ifndef _V3ERROR_NO_GLOBAL_
# include "V3Ast.h"
# include "V3Global.h"
# include "V3Stats.h"
#endif

//======================================================================
// Statics

FileLine FileLine::s_defaultFileLine = FileLine(EmptySecret());

int V3Error::s_errcnt = 0;
int V3Error::s_debugDefault = 0;
ostringstream V3Error::s_errorStr;		// Error string being formed
V3ErrorCode V3Error::s_errorCode = V3ErrorCode::FATAL;
bool V3Error::s_describedEachWarn[V3ErrorCode::MAX];
bool V3Error::s_describedWarnings = false;
bool V3Error::s_pretendError[V3ErrorCode::MAX];

struct v3errorIniter {
    v3errorIniter() {  V3Error::init(); };
};
v3errorIniter v3errorInit; 

//######################################################################
// ErrorCode class functions

V3ErrorCode::V3ErrorCode(const char* msgp) {
    // Return error encoding for given string, or ERROR, which is a bad code
    for (int codei=V3ErrorCode::FIRST_WARN; codei<V3ErrorCode::MAX; codei++) {
	V3ErrorCode code = (V3ErrorCode)codei;
	if (0==strcasecmp(msgp,code.ascii())) {
	    m_e = code; return;
	}
    }
    m_e = V3ErrorCode::ERROR;
}

//######################################################################
// FileLine class functions

void FileLine::lineDirective(const char* textp) {
    // Handle `line directive
    // Skip `line
    while (*textp && isspace(*textp)) textp++;
    while (*textp && !isspace(*textp)) textp++;
    while (*textp && (isspace(*textp) || *textp=='"')) textp++;

    // Grab linenumber
    const char *ln = textp;
    while (*textp && !isspace(*textp)) textp++;
    if (isdigit(*ln)) {
	this->lineno(atoi(ln));
    }
    while (*textp && (isspace(*textp) || *textp=='"')) textp++;

    // Grab filename
    const char *fn = textp;
    while (*textp && !(isspace(*textp) || *textp=='"')) textp++;
    if (textp != fn) {
	string strfn = fn;
	strfn = strfn.substr(0, textp-fn);
	this->filename(strfn);
    }
    //printf ("PPLINE %d '%s'\n", s_lineno, s_filename.c_str());
}

bool FileLine::warnOff(const string& msg, bool flag) {
    V3ErrorCode code (msg.c_str());
    if (code == V3ErrorCode::ERROR) {
	return false;
    } else {
	warnOff(code, flag);
	return true;
    }
}

FileLine* FileLine::copyOrSameFileLine() {
    // Return this, or a copy of this
    // There are often more then one token per line, thus we use the
    // same pointer as long as we're on the same line.
    static FileLine* lastNewp = NULL;
    if (lastNewp && *lastNewp == *this) {
	return lastNewp;
    }
    FileLine* newp = new FileLine(this);
    lastNewp = newp;
    return newp;
}

const string FileLine::filebasename() const {
    string name = filename();
    string::size_type pos;
    if ((pos = name.rfind("/")) != string::npos) {
	name.erase(0,pos+1);
    }
    return name;
}

const string FileLine::profileFuncname() const {
    // Return string that is OK as a function name - for profiling
    string name  = filebasename();
    string::size_type pos;
    if ((pos = name.find(".")) != string::npos) {
	name = name.substr(0,pos);
    }
    while ((pos = name.find_first_not_of("abcdefghijlkmnopqrstuvwxyzABCDEFGHIJLKMNOPQRSTUVWXYZ0123456789_"))
	   != string::npos) {
	name.replace(pos, 1, "_");
    }
    name += "__"+cvtToStr(lineno());
    return name;
}

string FileLine::ascii() const {
    return filename()+":"+cvtToStr(lineno());
}
ostream& operator<<(ostream& os, FileLine* fileline) {
    os <<fileline->ascii()<<": "<<hex;
    return(os);
}

bool FileLine::warnIsOff(V3ErrorCode code) {
    if (m_warnOff.test(code)) return true;
    // UNOPTFLAT implies UNOPT
    if (code==V3ErrorCode::UNOPT && m_warnOff.test(V3ErrorCode::UNOPTFLAT)) return true;
    return false;
}

void FileLine::v3errorEnd(ostringstream& str) {
    if (this && m_lineno) {
	ostringstream nsstr;
	nsstr<<this<<str.str();
	if (warnIsOff(V3Error::errorCode())) V3Error::suppressThisWarning();
	V3Error::v3errorEnd(nsstr);
    } else {
	V3Error::v3errorEnd(str);
    }
}
//######################################################################
// V3Error class functions

void V3Error::init() {
    for (int i=0; i<V3ErrorCode::MAX; i++) {
	s_describedEachWarn[i] = false;
	s_pretendError[i] = false;
    }
    pretendError(V3ErrorCode::BLKANDNBLK, true);

    if (string(V3ErrorCode(V3ErrorCode::MAX).ascii()) != " MAX") {
	v3fatalSrc("Enum table in V3ErrorCode::ascii() is munged");
    }
}

string V3Error::lineStr (const char* filename, int lineno) {
    ostringstream out;
    const char* fnslashp = strrchr (filename, '/');
    if (fnslashp) filename = fnslashp+1;
    out<<filename<<":"<<dec<<lineno<<":";
    const char* spaces = "                    ";
    int numsp = out.str().length(); if (numsp>20) numsp = 20;
    out<<(spaces + numsp);
    return out.str();
}

void V3Error::incErrors() {
    s_errcnt++;
    if (errorCount() == MAX_ERRORS) {  // Not >= as would otherwise recurse
	v3fatal ("Exiting due to too many errors encountered\n");
    }
}

void V3Error::abortIfErrors() {
    if (errorCount()) {
	v3fatal ("Exiting due to "<<dec<<errorCount()<<" warning(s)\n");
    }
}

string V3Error::msgPrefix(V3ErrorCode code) {
    if (code==V3ErrorCode::SUPPRESS) return "-arning-suppressed: ";
    else if (code==V3ErrorCode::FATAL) return "%Error: ";
    else if (code==V3ErrorCode::ERROR
	     || s_pretendError[code]) return "%Error: ";
    else return "%Warning-"+(string)code.ascii()+": ";
}

//======================================================================
// Global Functions

string V3Error::v3sform (const char* format, ...) {
    static char msg[1000] = "";

    va_list ap;
    va_start(ap,format);
    vsprintf(msg,format,ap);
    va_end(ap);

    string out = msg;
    return out;
}

void V3Error::suppressThisWarning() {
    if (s_errorCode>=V3ErrorCode::FIRST_WARN) {
	V3Stats::addStatSum(string("Warnings, Suppressed ")+s_errorCode.ascii(), 1);
	s_errorCode=V3ErrorCode::SUPPRESS;
    }
}

void V3Error::v3abort () {
    v3fatalSrc("v3abort called\n");
}

void V3Error::v3errorEnd (ostringstream& sstr) {
    if (s_errorCode!=V3ErrorCode::SUPPRESS || debug()) {
	cerr<<msgPrefix()<<sstr.str();
	if (sstr.str()[sstr.str().length()-1] != '\n') {
	    cerr<<endl;
	}
	if (s_errorCode!=V3ErrorCode::SUPPRESS) {
	    if (!s_describedEachWarn[s_errorCode]
		&& !s_pretendError[s_errorCode]) {
		s_describedEachWarn[s_errorCode] = true;
		if (s_errorCode>=V3ErrorCode::FIRST_WARN && !s_describedWarnings) {
		    cerr<<msgPrefix()<<"Use \"/* verilator lint_off "<<s_errorCode.ascii()
			<<" */\" and lint_on around source to disable this message."<<endl;
		    s_describedWarnings = true;
		}
		if (s_errorCode.dangerous()) {
		    cerr<<msgPrefix()<<"*** See the manual before disabling this,"<<endl;
		    cerr<<msgPrefix()<<"else you may end up with different sim results."<<endl;
		}
	    }
	    incErrors();
	    if (s_errorCode==V3ErrorCode::FATAL) {
		static bool inFatal = false;
		if (!inFatal) {
		    inFatal = true;
#ifndef _V3ERROR_NO_GLOBAL_
		    if (debug()) {
			v3Global.rootp()->dumpTreeFile(v3Global.debugFilename("final.tree",99));
			V3Stats::statsFinalAll(v3Global.rootp());
			V3Stats::statsReport();
		    }
#endif
		}
		abort();
//		exit(10);
	    }
	}
    }
}
