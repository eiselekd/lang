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
#include <list>

#include "V3Global.h"
#include "V3GraphAlg.h"

//######################################################################
//######################################################################
// Algorithms - delete

void V3Graph::deleteCutableOnlyEdges() {
    // Any vertices with only cutable edges will get deleted

    // Vertex::m_user begin: indicates can be deleted
    // Pass 1, mark those.  Don't delete now, as we don't want to rip out whole trees
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	vertexp->user(true);
	for (V3GraphEdge* edgep = vertexp->inBeginp(); edgep; edgep=edgep->inNextp()) {
	    if (!edgep->cutable()) {
		vertexp->user(false);	// Can't delete it
		break;
	    }
	}
	for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep=edgep->outNextp()) {
	    if (!edgep->cutable()) {
		vertexp->user(false);	// Can't delete it
		break;
	    }
	}
    }

    // Pass 2, delete those marked
    // Rather then doing a delete() we set the weight to 0 which disconnects the edge.
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	if (vertexp->user()) {
	    //UINFO(7,"Disconnect "<<vertexp->name()<<endl);
	    for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep=edgep->outNextp()) {
		edgep->cut();
	    }
	}
    }

    // Vertex::m_user end, now unused
}

//######################################################################
//######################################################################
// Algorithms - weakly connected components

class GraphRemoveRedundant : GraphAlg {
    bool	m_sumWeights;		///< Sum, rather then maximize weights
private:
    void main() {
	for (V3GraphVertex* vertexp = m_graphp->verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	    vertexIterate(vertexp);
	}
    }
    void vertexIterate(V3GraphVertex* vertexp) {
	// Clear marks
	for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep=edgep->outNextp()) {
	    edgep->top()->user(false);
	}
	// Mark edges and detect duplications
	for (V3GraphEdge* nextp, *edgep = vertexp->outBeginp(); edgep; edgep=nextp) {
	    nextp = edgep->outNextp();
	    if (followEdge(edgep)) {
		V3GraphVertex* outVertexp = edgep->top();
		V3GraphEdge* prevEdgep = (V3GraphEdge*)outVertexp->userp();
		if (!prevEdgep) { // No previous assignment
		    outVertexp->userp(edgep);
		} else { // Duplicate
		    bool saveOld = true;
		    if (prevEdgep->cutable() && !edgep->cutable()) {
			saveOld = false;  // new !cutable more important then old
		    } else if (!prevEdgep->cutable() && edgep->cutable()) {
			saveOld = true;  // old !cutable more important then new
		    } else {
			saveOld = true;
			if (!m_sumWeights && (prevEdgep->weight() < edgep->weight())) {  // Keep max weight
			    prevEdgep->weight(edgep->weight());
			}
		    }
		    if (saveOld) {
			if (m_sumWeights) prevEdgep->weight(prevEdgep->weight() + edgep->weight());
			edgep->unlinkDelete(); edgep = NULL;
		    } else {
			if (m_sumWeights) edgep->weight(prevEdgep->weight() + edgep->weight());
			prevEdgep->unlinkDelete(); prevEdgep = NULL;
			outVertexp->userp(edgep);
		    }
		}
	    }
	}
    }
public:
    GraphRemoveRedundant(V3Graph* graphp, V3EdgeFuncP edgeFuncp, bool sumWeights)
	: GraphAlg(graphp, edgeFuncp), m_sumWeights(sumWeights) {
	main();
    }
    ~GraphRemoveRedundant() {}
};

void V3Graph::removeRedundantEdges(V3EdgeFuncP edgeFuncp) {
    GraphRemoveRedundant (this, edgeFuncp, false);
}
void V3Graph::removeRedundantEdgesSum(V3EdgeFuncP edgeFuncp) {
    GraphRemoveRedundant (this, edgeFuncp, true);
}

//######################################################################
//######################################################################
// Algorithms - weakly connected components

class GraphAlgWeakly : GraphAlg {
private:
    void main() {
	// Initialize state
	m_graphp->clearColors();
	// Color graph
	uint32_t currentColor = 0;
	for (V3GraphVertex* vertexp = m_graphp->verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	    currentColor ++;
	    vertexIterate(vertexp, currentColor);
	}
    }

    void vertexIterate(V3GraphVertex* vertexp, uint32_t currentColor) {
	// Assign new color to each unvisited node
	// then visit each of its edges, giving them the same color
	if (vertexp->color()) return;  // Already colored it
	vertexp->color(currentColor);
	for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep=edgep->outNextp()) {
	    if (followEdge(edgep)) {
		vertexIterate(edgep->top(), currentColor);
	    }
	}
	for (V3GraphEdge* edgep = vertexp->inBeginp(); edgep; edgep=edgep->inNextp()) {
	    if (followEdge(edgep)) {
		vertexIterate(edgep->fromp(), currentColor);
	    }
	}
    }
public:
    GraphAlgWeakly(V3Graph* graphp, V3EdgeFuncP edgeFuncp)
	: GraphAlg(graphp, edgeFuncp) {
	main();
    }
    ~GraphAlgWeakly() {}
};

void V3Graph::weaklyConnected(V3EdgeFuncP edgeFuncp) {
    GraphAlgWeakly (this, edgeFuncp);
}

//######################################################################
//######################################################################
// Algorithms - strongly connected components

class GraphAlgStrongly : GraphAlg {
private:
    uint32_t	m_currentDfs;		// DFS count
    vector<V3GraphVertex*> m_callTrace;	// List of everything we hit processing so far

    void main() {
	// Use Tarjan's algorithm to find the strongly connected subgraphs.
	// Node State:
	//     Vertex::user  	// DFS number indicating possible root of subtree, 0=not iterated
	//     Vertex::color	// Output subtree number (fully processed)

	// Clear info
	for (V3GraphVertex* vertexp = m_graphp->verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	    vertexp->color(0);
	    vertexp->user(0);
	}
	// Color graph
	for (V3GraphVertex* vertexp = m_graphp->verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	    if (!vertexp->user()) {
		m_currentDfs++;
		vertexIterate(vertexp);
	    }
	}
	// If there's a single vertex of a color, it doesn't need a subgraph
	// This simplifies the consumer's code, and reduces graph debugging clutter
	for (V3GraphVertex* vertexp = m_graphp->verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	    bool onecolor = true;
	    for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep=edgep->outNextp()) {
		if (followEdge(edgep)) {
		    if (vertexp->color() == edgep->top()->color()) {
			onecolor = false;
			break;
		    }
		}
	    }
	    if (onecolor) vertexp->color(0);
	}
    }

    void vertexIterate(V3GraphVertex* vertexp) {
	uint32_t thisDfsNum = m_currentDfs++;
	vertexp->user(thisDfsNum);
	vertexp->color(0);
	for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep=edgep->outNextp()) {
	    if (followEdge(edgep)) {
		V3GraphVertex* top = edgep->top();
		if (!top->user()) {  // Dest not computed yet
		    vertexIterate(top);
		}
		if (!top->color()) { // Dest not in a component
		    if (vertexp->user() > top->user()) vertexp->user(top->user());
		}
	    }
	}
	if (vertexp->user() == thisDfsNum) { // New head of subtree
	    vertexp->color(thisDfsNum); // Mark as component
	    while (!m_callTrace.empty()) {
		V3GraphVertex* popVertexp = m_callTrace.back();
		if (popVertexp->user() >= thisDfsNum) { // Lower node is part of this subtree
		    m_callTrace.pop_back();
		    popVertexp->color(thisDfsNum);
		} else {
		    break;
		}
	    }
	} else { // In another subtree (maybe...)
	    m_callTrace.push_back(vertexp);
	}
    }
public:
    GraphAlgStrongly(V3Graph* graphp, V3EdgeFuncP edgeFuncp)
	: GraphAlg(graphp, edgeFuncp) {
	m_currentDfs = 0;
	main();
    }
    ~GraphAlgStrongly() {}
};

void V3Graph::stronglyConnected(V3EdgeFuncP edgeFuncp) {
    GraphAlgStrongly (this, edgeFuncp);
}

//######################################################################
//######################################################################
// Algorithms - ranking

class GraphAlgRank : GraphAlg {
private:
    void main() {
	// Rank each vertex, ignoring cutable edges
	// Vertex::m_user begin: 1 indicates processing, 2 indicates completed
	// Clear existing ranks
	for (V3GraphVertex* vertexp = m_graphp->verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	    vertexp->rank(0);
	    vertexp->user(0);
	}
	for (V3GraphVertex* vertexp = m_graphp->verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	    if (!vertexp->user()) {
		vertexIterate(vertexp,1);
	    }
	}
    }

    void vertexIterate(V3GraphVertex* vertexp, uint32_t currentRank) {
	// Assign rank to each unvisited node
	// If larger rank is found, assign it and loop back through
	// If we hit a back node make a list of all loops
	if (vertexp->user() == 1) {
	    m_graphp->reportLoops(m_edgeFuncp, vertexp);
	    m_graphp->loopsMessageCb(vertexp);
	    return;
	}
	if (vertexp->rank() >= currentRank) return;  // Already processed it
	vertexp->user(1);
	vertexp->rank(currentRank);
	for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep=edgep->outNextp()) {
	    if (followEdge(edgep)) {
		vertexIterate(edgep->top(),currentRank+1);
	    }
	}
	vertexp->user(2);
    }
public:
    GraphAlgRank(V3Graph* graphp, V3EdgeFuncP edgeFuncp)
	: GraphAlg(graphp, edgeFuncp) {
	main();
    }
    ~GraphAlgRank() {}
};

void V3Graph::rank() {
    GraphAlgRank (this, &V3GraphEdge::followAlwaysTrue);
}

void V3Graph::rank(V3EdgeFuncP edgeFuncp) {
    GraphAlgRank (this, edgeFuncp);
}

//######################################################################
//######################################################################
// Algorithms - ranking

class GraphAlgRLoops : GraphAlg {
private:
    vector<V3GraphVertex*> m_callTrace;	// List of everything we hit processing so far
    bool		   m_done;	// Exit algorithm

    void main(V3GraphVertex* vertexp) {
	// Vertex::m_user begin: 1 indicates processing, 2 indicates completed
	// Clear existing ranks
	m_graphp->userClearVertices();
	m_callTrace.reserve(100);
	vertexIterate(vertexp, 0);
    }

    void vertexIterate(V3GraphVertex* vertexp, uint32_t currentRank) {
	// Assign rank to each unvisited node
	// When we hit ourself again, return the list of all loops
	if (m_done) return;

	m_callTrace.reserve(currentRank+10);   // Leave slop for speed
	m_callTrace[currentRank++] = vertexp; 

	if (vertexp->user() == 1) {
	    for (unsigned i=0; i<currentRank; i++) {
		m_graphp->loopsVertexCb(m_callTrace[i]);
	    }
	    m_done = true;
	    return;
	}
	if (vertexp->user() == 2) return;  // Already processed it
	vertexp->user(1);
	for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep=edgep->outNextp()) {
	    if (followEdge(edgep)) {
		vertexIterate(edgep->top(),currentRank);
	    }
	}
	vertexp->user(2);
    }
public:
    GraphAlgRLoops(V3Graph* graphp, V3EdgeFuncP edgeFuncp, V3GraphVertex* vertexp)
	: GraphAlg(graphp, edgeFuncp) {
	m_done = false;
	main(vertexp);
    }
    ~GraphAlgRLoops() {}
};

void V3Graph::reportLoops(V3EdgeFuncP edgeFuncp, V3GraphVertex* vertexp) {
    GraphAlgRLoops (this, edgeFuncp, vertexp);
}

//######################################################################
//######################################################################
// Algorithms - make non cutable

void V3Graph::makeEdgesNonCutable(V3EdgeFuncP edgeFuncp) {
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	// Only need one direction, we'll always see the other at some point...
	for (V3GraphEdge* edgep = vertexp->inBeginp(); edgep; edgep = edgep->inNextp()) {
	    if (edgep->cutable() && edgep->weight() && (edgeFuncp)(edgep)) {
		edgep->cutable(false);
	    }
	}
    }
}

//######################################################################
//######################################################################
// Algorithms - ordering
//	Compute near optimal ordering of the nodes, where:
//	    If a required    edge is A->B, rank(A)<rank(B)
//	    Visit edges and assign ranks to keep minimal crossings
//		(Results in better dcache packing.)

struct GraphOrderVertexCmp {
    inline bool operator () (const V3GraphVertex* lhsp, const V3GraphVertex* rhsp) const {
	// LHS goes first if of lower rank, or lower fanout
	if (lhsp->m_rank < rhsp->m_rank) return 1;
	if (lhsp->m_rank > rhsp->m_rank) return 0;
	return (lhsp->m_fanout < rhsp->m_fanout);
    }
};
struct GraphOrderEdgeCmp {
    inline bool operator () (const V3GraphEdge* lhsp, const V3GraphEdge* rhsp) const {
	if (!lhsp->m_weight || !rhsp->m_weight) return 0;
	GraphOrderVertexCmp cmp;
	return (cmp(lhsp->m_top, rhsp->m_top));
    }
};

//--------------------------------------------------------------------

void V3Graph::order() {
    UINFO(2,"Order:\n");

    // Compute rankings again
    rank(&V3GraphEdge::followAlwaysTrue);

    // Compute fanouts
    // Vertex::m_user begin: 1 indicates processing, 2 indicates completed
    userClearVertices();
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	if (!vertexp->user()) {
	    orderDFSIterate(vertexp);
	}
    }
    // Speed up subsequent accesses.
    { // Sort list of vertices by rank, then fanout
	vector<V3GraphVertex*> vertices;
	for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	    vertices.push_back(vertexp);
	}
	sort(vertices.begin(), vertices.end(), GraphOrderVertexCmp());
	this->verticesUnlink();
	for (vector<V3GraphVertex*>::iterator it = vertices.begin(); it!=vertices.end(); ++it) {
	    (*it)->verticesPushBack(this);
	}
    }

    // Sort edges by rank then fanout of node they point to
    vector<V3GraphEdge*> edges;
    for (V3GraphVertex* vertexp = verticesBeginp(); vertexp; vertexp=vertexp->verticesNextp()) {
	// Make a vector
	for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep = edgep->outNextp()) {
	    edges.push_back(edgep);
	}
	// Sort
	sort(edges.begin(), edges.end(), GraphOrderEdgeCmp());
	// Extract
	vertexp->outUnlink();
	for (vector<V3GraphEdge*>::iterator it = edges.begin(); it!=edges.end(); ++it) {
	    (*it)->outPushBack();
	}
	// Prep for next
	edges.clear();
    }
}

double V3Graph::orderDFSIterate(V3GraphVertex* vertexp) {
    // Compute fanouts of each node
    // If forward edge, don't double count that fanout
    if (vertexp->user() == 2) return vertexp->fanout();  // Already processed it
    if (vertexp->user() == 1) v3fatalSrc("Loop found, backward edges should be dead\n");
    vertexp->user(1);
    double fanout = 0;
    for (V3GraphEdge* edgep = vertexp->outBeginp(); edgep; edgep = edgep->outNextp()) {
	if (edgep->weight()) fanout += orderDFSIterate(edgep->m_top);
    }
    // Just count inbound edges
    for (V3GraphEdge* edgep = vertexp->inBeginp(); edgep; edgep = edgep->inNextp()) {
	if (edgep->weight()) fanout ++;
    }
    vertexp->fanout(fanout);
    vertexp->user(2);
    return vertexp->fanout();
}
