#include "common.h"

SEXP R_scalarInteger(int v)
{
  SEXP  ans = allocVector(INTSXP, 1);
  INTEGER(ans)[0] = v;
  return(ans);
}

SEXP
R_scalarLogical(Rboolean v)
{
  SEXP  ans = allocVector(LGLSXP, 1);
  LOGICAL(ans)[0] = v;
  return(ans);
}

static SEXP Rgraphviz_graph_type_tag;

#define CHECK_Rgraphviz_graph(s) do { \
     if (TYPEOF(s) != EXTPTRSXP || \
         R_ExternalPtrTag(s) != Rgraphviz_graph_type_tag) \
         error("bad graph reference"); \
} while (0)

SEXP Rgraphviz_init(void) {
    Rgraphviz_graph_type_tag = install("RGRAPH_TYPE_TAG");
    return(R_NilValue);
}

SEXP Rgraphviz_fin(SEXP s) { 
    Agraph_t *g;

    CHECK_Rgraphviz_graph(s);
    g = R_ExternalPtrAddr(s);
    agclose(g);
    R_ClearExternalPtr(s);
    return(R_NilValue);
}

SEXP Rgraphviz_graph2ps(SEXP graph, SEXP outFile) {
    Agraph_t *g;
    SEXP slotTmp;
    FILE* of;

    slotTmp = GET_SLOT(graph, install("agraph"));
    CHECK_Rgraphviz_graph(slotTmp);
    g = R_ExternalPtrAddr(slotTmp);
	
    if (!isString(outFile))
	error("outFile must be a file name");

    of = fopen(STR(outFile),"w");
    if (of == NULL) 
	error("Error opening file");
    
    Output_lang = POSTSCRIPT;
    Output_file = of;
    CodeGen = &PS_CodeGen;
    
    dotneato_set_margins(g);
    Rprintf("Now outputting graph to %s\n", STR(outFile));
    emit_graph(g,0);  
    dot_cleanup(g);
    fclose(of);
    emit_reset(g);
    return(R_NilValue);
}


SEXP Rgraphviz_agopen(SEXP name, SEXP kind, SEXP nodes, SEXP eList) {
    Agraph_t *g;
    Agnode_t *head, *tail;
    Agedge_t *curEdge;
    SEXP graphRef, elmt, obj, klass;
    int ag_k = 0;
    int i, curNode;

    if (!isInteger(kind))
	error("kind must be an integer value");
    else
	ag_k = INTEGER(kind)[0];

    if (!isString(name))
	error("name must be a string");

    if (!isNewList(eList))
	error("nelist must be a list");

    aginit();
    g = agopen(STR(name), ag_k);

    /* Get the nodes created */
    for (i = 0; i < length(nodes); i++) {
	agnode(g, CHAR(STRING_ELT(nodes,i)));
    }

    /* now fill in the edges */
    for (i = 0; i < length(eList); i++) {
	PROTECT(elmt = VECTOR_ELT(eList, i));
	curNode = INTEGER(GET_SLOT(elmt, 
				   Rf_install("bNode")))[0];
	head = agfindnode(g, CHAR(STRING_ELT(nodes,curNode-1)));
	if (head == NULL)
	    error("Missing head node");
	/* Get weights for these edges */
	curNode = INTEGER(GET_SLOT(elmt, 
				   Rf_install("eNode")))[0]; 
	tail = agfindnode(g,CHAR(STRING_ELT(nodes,curNode-1)));
	if (tail == NULL)
	    error("Missing tail node");
	if (agfindedge(g,head,tail) == NULL) {  
	    curEdge = agedge(g, tail, head);
	    curEdge->u.weight = INTEGER(GET_SLOT(elmt,
						 Rf_install("weight")))[0];
	}  
	UNPROTECT(1);
    }

    PROTECT(graphRef = R_MakeExternalPtr(g,Rgraphviz_graph_type_tag,
				 R_NilValue));
    R_RegisterCFinalizer(graphRef, (R_CFinalizer_t)Rgraphviz_fin);

    klass = MAKE_CLASS("Ragraph");
    /*XX  the call to duplicate is needed until 1.6.2 is released
      because of a bug in the NEW() mechanism in < 1.6.2! */
    PROTECT(obj = duplicate(NEW_OBJECT(klass)));
    
    SET_SLOT(obj, Rf_install("agraph"), graphRef);
    SET_SLOT(obj, Rf_install("laidout"), R_scalarLogical(FALSE));
    SET_SLOT(obj, Rf_install("numEdges"), R_scalarInteger(length(eList)));

    UNPROTECT(2);

    return(obj);
}

SEXP Rgraphviz_doDotLayout(SEXP graph) {
    Agraph_t *g;
    Rboolean laidout;
    SEXP slotTmp, pos, cPoints;

    laidout = (int)LOGICAL(GET_SLOT(graph, Rf_install("laidout")))[0];
    if (laidout == FALSE) {
	slotTmp = GET_SLOT(graph, install("agraph"));
	CHECK_Rgraphviz_graph(slotTmp);
	g = R_ExternalPtrAddr(slotTmp);
	
	g = dotLayout(g);
	PROTECT(pos = getNodeLocs(g));
	PROTECT(cPoints= 
		getEdgeLocs(g, INTEGER(GET_SLOT(graph, 
						Rf_install("numEdges")))[0]));
	
	PROTECT(slotTmp = R_MakeExternalPtr(g,Rgraphviz_graph_type_tag,
					     R_NilValue));
	R_RegisterCFinalizer(slotTmp, (R_CFinalizer_t)Rgraphviz_fin);
	SET_SLOT(graph, Rf_install("agraph"), slotTmp);
	SET_SLOT(graph,Rf_install("nodeLocs"),pos);
	SET_SLOT(graph,Rf_install("laidout"), R_scalarLogical(TRUE));
	SET_SLOT(graph,Rf_install("edgePoints"), cPoints);
	UNPROTECT(3);
    }
    return(graph);
}

SEXP getNodeLocs(Agraph_t *g) {
    Agnode_t *node;
    SEXP pos;
    int i, nodes;

    nodes = agnnodes(g);
    node = agfstnode(g);
    PROTECT(pos = allocMatrix(REALSXP, nodes, 2));
    for (i = 0; i < nodes; i++) {
	REAL(pos)[i] = node->u.coord.x;
	REAL(pos)[i + nodes] = node->u.coord.y;
	node = agnxtnode(g,node);
    }
    UNPROTECT(1);
    return(pos);
}

SEXP getEdgeLocs(Agraph_t *g, int numEdges) {
    SEXP outList, curCP, curEP, pntList, pntSet, curXY;
    SEXP epClass, cpClass, xyClass;
    Agnode_t *node;
    Agedge_t *edge;
    bezier bez;
    int nodes, numEl;
    int i,k,l,pntLstEl;
    int curEle = 0;

    epClass = MAKE_CLASS("edgePoints");
    cpClass = MAKE_CLASS("controlPoints");
    xyClass = MAKE_CLASS("xyPoint");

    PROTECT(outList = allocVector(VECSXP, numEdges));

    nodes = agnnodes(g);
    node = agfstnode(g);
    for (i = 0; i < nodes; i++) {
	edge = agfstout(g, node);
	while (edge != NULL) {
	    PROTECT(curEP = duplicate(NEW_OBJECT(epClass)));
	    bez = edge->u.spl->list[0];
	    PROTECT(pntList = allocVector(VECSXP, 
					  ((bez.size-1)/3)));
	    pntLstEl = 0;

	    /* There are really (bez.size-1)/3 sets of control */
	    /* points, with the first set containing teh first 4 */
	    /* points, and then every other set starting with the */
	    /* last point from the previous set and then the next 3 */
	    for (k = 1; k < bez.size; k += 3) {
		PROTECT(curCP = duplicate(NEW_OBJECT(cpClass)));
		PROTECT(pntSet = allocVector(VECSXP, 4));
		for (l = -1; l < 3; l++) {
		    PROTECT(curXY = duplicate(NEW_OBJECT(xyClass)));
		    SET_SLOT(curXY, Rf_install("x"), 
			     R_scalarInteger(bez.list[k+l].x));
		    SET_SLOT(curXY, Rf_install("y"), 
			     R_scalarInteger(bez.list[k+l].y));
		    SET_ELEMENT(pntSet, l+1, curXY);
		    UNPROTECT(1);
		}
		SET_SLOT(curCP, Rf_install("cPoints"), pntSet);
		SET_ELEMENT(pntList, pntLstEl++, curCP);
		UNPROTECT(2);
	    }	    
	    SET_SLOT(curEP, Rf_install("splines"), pntList);
	    SET_ELEMENT(outList, curEle++, curEP);
	    UNPROTECT(2);
	    edge = agnxtout(g, edge);
	}
	node = agnxtnode(g, node);
    }
    UNPROTECT(1);

    return(outList);
}

Agraph_t *dotLayout(Agraph_t *g) {
    graph_init(g);    

    g->u.drawing->engine = DOT;
    dot_init_node_edge(g);
    dot_rank(g);
    dot_mincross(g);
    dot_position(g);
    dot_sameports(g);
    dot_splines(g);
    dotneato_postprocess(g, dot_nodesize);

    return(g);
}

SEXP Rgraphviz_getDotfile(SEXP graph) {
    /* !! Currently writes to stdout */
    Agraph_t *g;

    CHECK_Rgraphviz_graph(graph);
    g = R_ExternalPtrAddr(graph);

    graph_init(g);
    aginit();

    attach_attrs(g);
    agwrite(g,stdout);
    return(R_NilValue);
}

SEXP Rgraphviz_emitGraph(SEXP graph, SEXP outFile) {
    Agraph_t *g;
    FILE* of;

    CHECK_Rgraphviz_graph(graph);
    g = R_ExternalPtrAddr(graph);
    if (!isString(outFile))
	error("outFile must be a file name");

    of = fopen(STR(outFile),"w");
    if (of == NULL) 
	error("Error opening file");

    graph_init(g);  

    Output_lang = POSTSCRIPT;
    Output_file = of;
    CodeGen = &PS_CodeGen;

    dotneato_set_margins(g);
    Rprintf("Now outputting graph to %s\n", STR(outFile));
    emit_graph(g,0);  
    dot_cleanup(g);
    fclose(of);
    emit_reset(g);
    return(R_NilValue);
}


