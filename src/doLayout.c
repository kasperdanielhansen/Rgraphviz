#include "common.h"
#include "util.h"

#ifdef GRAPHVIZGT_2_4
static char *layouts[] = { "dot", "neato", "twopi", "circo", "fdp"};
#else
enum {
        DOTLAYOUT = 0,
        NEATOLAYOUT,
        TWOPILAYOUT,
        CIRCOLAYOUT,
        FDPLAYOUT
};
#endif

SEXP Rgraphviz_doLayout(SEXP graph, SEXP layoutType) {
    /* Will perform a Graphviz layout on a graph */
    
	Agraph_t *g;
	Rboolean laidout;
	SEXP slotTmp, nLayout, cPoints, bb;

	/* First make sure that hte graph is not already laid out */
	laidout = (int)LOGICAL(GET_SLOT(graph, Rf_install("laidout")))[0];
	if (laidout == FALSE) {
		/* Extract the Agraph_t pointer from the S4 object */
		PROTECT(slotTmp = GET_SLOT(graph, install("agraph")));
		CHECK_Rgraphviz_graph(slotTmp);
		g = R_ExternalPtrAddr(slotTmp);

		/* Call the appropriate Graphviz layout routine */
		if (!isInteger(layoutType))
			error("layoutType must be an integer value");
		else {
			/* Note that we're using the standard dotneato */
			/* layout commands for layouts and not the ones */
			/* provided below.  This is a test */
#ifdef GRAPHVIZGT_2_4
			gvLayout(gvc, g, layouts[INTEGER(layoutType)[0]]);
#else
			switch(INTEGER(layoutType)[0]) {
			case DOTLAYOUT:
				dot_layout(g);
				break;
			case NEATOLAYOUT:
				neato_layout(g);
				break;
			case TWOPILAYOUT:
				twopi_layout(g);
				break;
			case CIRCOLAYOUT:
				circo_layout(g);
				break;
#ifndef Win32
			case FDPLAYOUT:
				fdp_layout(g);
				break;
#endif
			default:
				error("Invalid layout type\n");
			}
#endif
		}
		
		/* Here we want to extract information for the resultant S4
		   object */
		PROTECT(nLayout = getNodeLayouts(g));
		PROTECT(bb = getBoundBox(g));
		PROTECT(cPoints= 
			getEdgeLocs(g, INTEGER(GET_SLOT(graph, 
							Rf_install("numEdges")))[0]));
		SET_SLOT(graph, Rf_install("agraph"), slotTmp);
		SET_SLOT(graph,Rf_install("AgNode"),nLayout);
		SET_SLOT(graph,Rf_install("laidout"), R_scalarLogical(TRUE));
		SET_SLOT(graph,Rf_install("AgEdge"), cPoints);
		SET_SLOT(graph,Rf_install("boundBox"), bb);
		UNPROTECT(4);
	}
	
	return(graph);
}

