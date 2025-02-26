// $Id$
//*************************************************************************
// DESCRIPTION: Verilator: Graph optimizations
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

#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <algorithm>
#include <vector>
#include <map>
#include <memory>

#include "V3Global.h"
#include "V3File.h"
#include "V3Graph.h"

int V3Graph::s_debug = 0;
int V3Graph::debug() { return max(V3Error::debugDefault(), s_debug); }

//######################################################################
//######################################################################
// Vertices

V3GraphVertex::V3GraphVertex(V3Graph* graphp)
    : m_fanout(0), m_color(0), m_rank(0)
{
    m_userp = NULL;
    verticesPushBack(graphp);
}

void V3GraphVertex::verticesPushBack(V3Graph* graphp) {
    m_vertices.pushBack(graphp->m_vertices, this);
}

void V3GraphVertex::unlinkEdges(V3Graph* graphp) {
    for (V3GraphEdge* edgep = outBeginp(); edgep; /*BELOW*/) {
	V3GraphEdge* nextp = edgep->outNextp();
	edgep->unlinkDelete();
	edgep = nextp;
    }
    for (V3GraphEdge* edgep = inBeginp(); edgep; /*BELOW*/) {
	V3GraphEdge* nextp = edgep->inNextp();
	edgep->unlinkDelete();
	edgep = nextp;
    }
}

void V3GraphVertex::unlinkDelete(V3Graph* graphp) {
    // Delete edges
    unlinkEdges(graphp);
    // Unlink from vertex list
    m_vertices.unlink(graphp->m_vertices, this);
    // Delete
    delete this; //this=NULL;
}

void V3GraphVertex::rerouteEdges(V3Graph* graphp) {
    // Make new edges for each from/to pair
    for (V3GraphEdge* iedgep = inBeginp(); iedgep; iedgep=iedgep->inNextp()) {
	for (V3GraphEdge* oedgep = outBeginp(); oedgep; oedgep=oedgep->outNextp()) {
	    new V3GraphEdge (graphp, iedgep->fromp(), oedgep->top(),
			     min(iedgep->weight(),oedgep->weight()),
			     iedgep->cutable() && oedgep->cutable());
	}
    }
    // Remove old edges
    unlinkEdges(graphp);
}

bool V3GraphVertex::inSize1() const {
    return !inEmpty() && inBeginp()->inNextp()==NULL;
}

bool V3GraphVertex::outSize1() const {
    return !outEmpty() && outBeginp()->outNextp()==NULL;
}

ostream& operator<<(ostream& os, V3GraphVertex* vertexp) {
    os<<"  VERTEX="<<vertexp->name();
    if (vertexp->rank()) os<<" r"<<vertexp->rank();
    if (vertexp->fanout()) os<<" f"<<vertexp->fanout();
    if (vertexp->color()) os<<" c"<<vertexp->color();
    return os;
}

//######################################################################
//######################################################################
// Edges

V3GraphEdge::V3GraphEdge(V3Graph* graphp, 
			 V3GraphVertex* fromp, V3GraphVertex* top, int weight,
			 bool cutable) {
    UASSERT(fromp, "Null from pointer\n");
    UASSERT(top, "Null to pointer\n");
    m_fromp = fromp;
    m_top = top;
    m_weight = weight;
    m_cutable = cutable;
    m_userp = NULL;
    // Link vertices to this edge
    outPushBack();
    inPushBack();
}

void V3GraphEdge::unlinkDelete() {
    // Unlink from side
    m_outs.unlink(m_fromp->m_outs, this);
    // Unlink to side
    m_ins.unlink(m_top->m_ins, this);
    // Delete
    delete this; //this=NULL;
}

void V3GraphEdge::outPushBack() {
    // m_fromp->m_outsp.push_back(this);
    m_outs.pushBack(m_fromp->m_outs, this);
}

void V3GraphEdge::inPushBack() {
    // m_top->m_insp.push_back(this);
    m_ins.pushBack(m_top->m_ins, this);
}

//######################################################################
//######################################################################
// Graph top level
// Constructors

V3Graph::V3Graph() {
    // Anything here is probably needed in clear() also
    verticesUnlink();
}

V3Graph::~V3Graph() {
    clear();
}

void V3Graph::clear() {
    // Empty it of all points, as if making a new object
    // Delete the old edges
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; /*BELOW*/) {
	    V3GraphEdge* nextp = edgep->outNextp();
	    delete edgep;
	    edgep = nextp;
	}
	vertexp->outUnlink();
    }
    // Delete the old vertices
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; /*BELOW*/) {
	V3GraphVertex* nextp = vertexp->verticesNextp();
	delete vertexp;
	vertexp = nextp;
    }
    verticesUnlink();
}

void V3Graph::userClearVertices() {
    // Clear user() in all of tree
    // We may use the userCnt trick in V3Ast later... For now we don't call this often, and
    // the extra code on each read of user() would probably slow things down more then help.
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	vertexp->user(0);
    }
}

void V3Graph::userClearEdges() {
    // Clear user() in all of tree
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep=edgep->outNextp()) {
	    edgep->userp(NULL);
	}
    }
}

void V3Graph::clearColors() {
    // Reset colors
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	vertexp->m_color = 0;
    }
}

//======================================================================
// Dumping

void V3Graph::loopsVertexCb(V3GraphVertex* vertexp) {
    // Needed here as V3GraphVertex<< isn't defined until later in header
    cout<<"-Info-Loop: "<<(void*)(vertexp)<<" "<<vertexp<<endl;
}

void V3Graph::dump(ostream& os) {
    // This generates a file used by graphviz, http://www.graphviz.org
    os<<" Graph:\n";
    // Print vertices
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	os<<"\tNode: "<<vertexp->name();
	if (vertexp->color()) os<<"  color="<<vertexp->color();
	os<<endl;
	// Print edges
	for (V3GraphEdge* edgep = vertexp->inBeginp(); edgep; edgep=edgep->inNextp()) {
	    dumpEdge (os, vertexp, edgep);
	}
	for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep=edgep->outNextp()) {
	    dumpEdge (os, vertexp, edgep);
	}
    }
}

void V3Graph::dumpEdge(ostream& os, V3GraphVertex* vertexp, V3GraphEdge* edgep) {
    if (edgep->weight()
	&& (edgep->fromp() == vertexp
	    || edgep->top() == vertexp)) {
	os<<"\t\t";
	if (edgep->fromp() == vertexp) os << "-> "<<edgep->top()->name();
	if (edgep->top() == vertexp) os << "<- "<<edgep->fromp()->name();
	if (edgep->cutable()) os<<"  [CUTABLE]";
	os<<endl;
    }
}

void V3Graph::dumpDotFilePrefixed(const string& nameComment, bool colorAsSubgraph) {
    if (v3Global.opt.dumpTree()) {
	dumpDotFile(v3Global.debugFilename(nameComment)+".dot", colorAsSubgraph);
    }
}

void V3Graph::dumpDotFile(const string& filename, bool colorAsSubgraph) {
    // This generates a file used by graphviz, http://www.graphviz.org
    // "hardcoded" parameters:
    const auto_ptr<ofstream> logp (V3File::new_ofstream(filename));
    if (logp->fail()) v3fatalSrc("Can't write "<<filename);

    // Header
    *logp<<"digraph v3graph {\n";
    *logp<<"\trankdir="<<dotRankDir()<<"\n";
    *logp<<"\tsize="<<"\"7.5,10\""<<"\n";

    // List of all possible subgraphs
    typedef multimap<string,V3GraphVertex*> SubgraphMmap;
    SubgraphMmap subgraphs;
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	string vertexSubgraph = (colorAsSubgraph && vertexp->color()) ? cvtToStr(vertexp->color()) : "";
	subgraphs.insert(make_pair(vertexSubgraph, vertexp));
    }

    // We use a map here, as we don't want to corrupt anything (userp) in the graph,
    // and we don't care if this is slow.
    map<V3GraphVertex*,int>  numMap;

    // Print vertices
    int n=0;
    string subgr;
    for (SubgraphMmap::iterator it = subgraphs.begin(); it!=subgraphs.end(); ++it) {
	string vertexSubgraph = it->first;
	V3GraphVertex* vertexp = it->second;
	numMap[vertexp] = n;
	if (subgr != vertexSubgraph) {
	    if (subgr!="") *logp<<"\t};\n";
	    subgr = vertexSubgraph;
	    if (subgr!="") *logp<<"\tsubgraph cluster_"<<subgr<<" {\n";
	}
	if (subgr!="") *logp<<"\t";
	*logp<<"\tn"<<vertexp->dotName()<<(n++)
	     <<"\t[fontsize=8 "
	     <<"label=\""<<(vertexp->name()!="" ? vertexp->name() : "\\N");
	if (vertexp->rank()) *logp<<" r"<<vertexp->rank();
	if (vertexp->fanout()) *logp<<" f"<<vertexp->fanout();
	if (vertexp->color()) *logp<<"\\n c"<<vertexp->color();
	*logp<<"\"";
	*logp<<", color="<<vertexp->dotColor();
	if (vertexp->dotStyle()!="") *logp<<", style="<<vertexp->dotStyle();
	if (vertexp->dotShape()!="") *logp<<", shape="<<vertexp->dotShape();
	*logp<<"];\n";
    }
    if (subgr!="") *logp<<"\t};\n";

    // Print edges
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep=edgep->outNextp()) {
	    if (edgep->weight()) {
		int fromVnum = numMap[edgep->fromp()];
		int toVnum   = numMap[edgep->top()];
		*logp<<"\tn"<<edgep->fromp()->dotName()<<fromVnum
		     <<" -> n"<<edgep->top()->dotName()<<toVnum
		     <<" ["
		    //<<"fontsize=8 label=\""<<(edgep->name()!="" ? edgep->name() : "\\E")<<"\""
		     <<"fontsize=8 label=\""<<(edgep->dotLabel()!="" ? edgep->dotLabel() : "")<<"\""
		     <<" weight="<<edgep->weight()
		     <<" color="<<edgep->dotColor();
		if (edgep->dotStyle()!="") *logp<<" style="<<edgep->dotStyle();
		//if (edgep->cutable()) { *logp<<",constraint=false"; }    // to rank without following edges
		*logp<<"];\n";
	    }
	}
    }
    // Vertex::m_user end, now unused

    // Trailer
    *logp << "}\n";
    logp->close();

    cout << "dot -Tps -o ~/a.ps "<<filename<<endl;
}
