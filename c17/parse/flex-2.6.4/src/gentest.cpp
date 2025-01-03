/* gentest.cpp - generate tests */

/*  Copyright (c) 1990 The Regents of the University of California. */
/*  All rights reserved. */

/*  This code is derived from software contributed to Berkeley by */
/*  Vern Paxson. */

/*  The United States Government has rights in this work pursuant */
/*  to contract no. DE-AC03-76SF00098 between the United States */
/*  Department of Energy and the University of California. */

/*  This file is part of flex. */

/*  Redistribution and use in source and binary forms, with or without */
/*  modification, are permitted provided that the following conditions */
/*  are met: */

/*  1. Redistributions of source code must retain the above copyright */
/*     notice, this list of conditions and the following disclaimer. */
/*  2. Redistributions in binary form must reproduce the above copyright */
/*     notice, this list of conditions and the following disclaimer in the */
/*     documentation and/or other materials provided with the distribution. */

/*  Neither the name of the University nor the names of its contributors */
/*  may be used to endorse or promote products derived from this software */
/*  without specific prior written permission. */

/*  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR */
/*  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR */
/*  PURPOSE. */

#include "flexdef.h"
#include <vector>
#include <string>
#include <map>
#include <tuple>

using namespace std;

typedef vector<string> strvec;

struct matchset {
    strvec l;
};

typedef map<int, matchset*> matchmap;

matchmap matches;

extern "C" {

int gentest_link_machines(int a, int b) {
    int ka, kb; matchset *va, *vb;
    if (a == NIL) {
    }
    else if (b == NIL) {
    }
    else {
	if (matches.count(a) && matches.count(b)) {
	    tie(ka,va) = *(matches.find(a));
	    tie(kb,vb) = *(matches.find(b));
	} else {
	}
    }
    return 0;
}


}
