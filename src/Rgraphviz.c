#include "common.h"

SEXP R_scalarReal(double v) {
    SEXP ans = allocVector(REALSXP,1);
    REAL(ans)[0] = v;
    return(ans);
}

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


SEXP Rgraphviz_agopen(SEXP name, SEXP kind, SEXP nodes, SEXP from,
		      SEXP to, SEXP weights) {
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

    if ((ag_k < 0)||(ag_k > 3))
	error("kind must be an integer value between 0 and 3");

    if (!isString(name))
	error("name must be a string");

    aginit();
    /* We don't actually want to do the layout with the directed */
    /* flag as it messes up the R edge drawing.  But we do need */
    /* to know later on if the graph is directed or not */
    g = agopen(STR(name), AGRAPH);

    /* Set default attributes */
    /* !!!! This is somewhat temporary until we allow */
    /* !!!! for all attributes to be settable in R */
    if (!agfindattr(g->proto->n,"shape"))
	agnodeattr(g,"shape","circle");

    /* Get the nodes created */
    for (i = 0; i < length(nodes); i++) {
	agnode(g, CHAR(STRING_ELT(nodes,i)));
    }
    /* now fill in the edges */
    for (i = 0; i < length(from); i++) {
	curNode = INTEGER(from)[i];
	head = agfindnode(g, CHAR(STRING_ELT(nodes,curNode-1)));
	if (head == NULL)
	    error("Missing head node");
	/* Get weights for these edges */
	curNode = INTEGER(to)[i];
	fflush(stdout);
	tail = agfindnode(g,CHAR(STRING_ELT(nodes,curNode-1)));
	if (tail == NULL)
	    error("Missing tail node");
	if ((ag_k == AGDIGRAPH) || (agfindedge(g,head,tail) == NULL)) {  
		curEdge = agedge(g, tail, head);
		curEdge->u.weight = INTEGER(weights)[i];
	}
    }

    PROTECT(graphRef = R_MakeExternalPtr(g,Rgraphviz_graph_type_tag,
				 R_NilValue));
    R_RegisterCFinalizer(graphRef, (R_CFinalizer_t)Rgraphviz_fin);

    klass = MAKE_CLASS("Ragraph");
    PROTECT(obj = NEW_OBJECT(klass));
    
    SET_SLOT(obj, Rf_install("agraph"), graphRef);
    SET_SLOT(obj, Rf_install("laidout"), R_scalarLogical(FALSE));
    SET_SLOT(obj, Rf_install("numEdges"), R_scalarInteger(agnedges(g)));

    UNPROTECT(2);

    return(obj);
}

SEXP Rgraphviz_doDotLayout(SEXP graph) {
    Agraph_t *g;
    Rboolean laidout;
    SEXP slotTmp, nLayout, cPoints, bb;

    laidout = (int)LOGICAL(GET_SLOT(graph, Rf_install("laidout")))[0];
    if (laidout == FALSE) {
	slotTmp = GET_SLOT(graph, install("agraph"));
	CHECK_Rgraphviz_graph(slotTmp);
	g = R_ExternalPtrAddr(slotTmp);
	
	g = dotLayout(g);
	PROTECT(nLayout = getNodeLayouts(g));
	PROTECT(bb = getBoundBox(g));
	PROTECT(cPoints= 
		getEdgeLocs(g, INTEGER(GET_SLOT(graph, 
						Rf_install("numEdges")))[0]));
	PROTECT(slotTmp = R_MakeExternalPtr(g,Rgraphviz_graph_type_tag,
					     R_NilValue));
	R_RegisterCFinalizer(slotTmp, (R_CFinalizer_t)Rgraphviz_fin);
	SET_SLOT(graph, Rf_install("agraph"), slotTmp);
	SET_SLOT(graph,Rf_install("nodes"),nLayout);
	SET_SLOT(graph,Rf_install("laidout"), R_scalarLogical(TRUE));
	SET_SLOT(graph,Rf_install("edgePoints"), cPoints);
	SET_SLOT(graph,Rf_install("boundBox"), bb);
	UNPROTECT(4);
    }
    return(graph);
}

SEXP getBoundBox(Agraph_t *g) {
    SEXP bbClass, xyClass, curBB, LLXY, URXY;

    xyClass = MAKE_CLASS("xyPoint");
    bbClass = MAKE_CLASS("boundingBox");

    PROTECT(curBB = NEW_OBJECT(bbClass));
    PROTECT(LLXY = NEW_OBJECT(xyClass));
    PROTECT(URXY = NEW_OBJECT(xyClass));

    SET_SLOT(LLXY,Rf_install("x"),R_scalarInteger(g->u.bb.LL.x));
    SET_SLOT(LLXY,Rf_install("y"),R_scalarInteger(g->u.bb.LL.y));
    SET_SLOT(URXY,Rf_install("x"),R_scalarInteger(g->u.bb.UR.x));
    SET_SLOT(URXY,Rf_install("y"),R_scalarInteger(g->u.bb.UR.y));

    SET_SLOT(curBB,Rf_install("botLeft"), LLXY);
    SET_SLOT(curBB,Rf_install("upRight"), URXY);

    UNPROTECT(3);
    return(curBB);
}

SEXP getNodeLayouts(Agraph_t *g) {
    Agnode_t *node;
    SEXP outLst, nlClass, xyClass, curXY, curNL;
    int i, nodes;
    
    nlClass = MAKE_CLASS("NodePosition");
    xyClass = MAKE_CLASS("xyPoint");

    nodes = agnnodes(g);
    node = agfstnode(g);
    PROTECT(outLst = allocVector(VECSXP, nodes));

    for (i = 0; i < nodes; i++) {	
	PROTECT(curNL = NEW_OBJECT(nlClass));
	PROTECT(curXY = NEW_OBJECT(xyClass));
	SET_SLOT(curXY,Rf_install("x"),R_scalarInteger(node->u.coord.x));
	SET_SLOT(curXY,Rf_install("y"),R_scalarInteger(node->u.coord.y));
	SET_SLOT(curNL,Rf_install("center"),curXY);
	SET_SLOT(curNL,Rf_install("height"),R_scalarInteger(node->u.ht));
	SET_SLOT(curNL,Rf_install("rWidth"),R_scalarInteger(node->u.rw));
	SET_SLOT(curNL,Rf_install("lWidth"),R_scalarInteger(node->u.lw));
	SET_ELEMENT(outLst, i, curNL);
	node = agnxtnode(g,node);
	UNPROTECT(2);
    }
    UNPROTECT(1);
    return(outLst);
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
    cpClass = MAKE_CLASS("BezierCurve");
    xyClass = MAKE_CLASS("xyPoint");

    PROTECT(outList = allocVector(VECSXP, numEdges));

    nodes = agnnodes(g);
    node = agfstnode(g);
    for (i = 0; i < nodes; i++) {
	edge = agfstout(g, node);
	while (edge != NULL) {
	    PROTECT(curEP = NEW_OBJECT(epClass));
	    bez = edge->u.spl->list[0];
	    PROTECT(pntList = allocVector(VECSXP, 
					  ((bez.size-1)/3)));
	    pntLstEl = 0;

	    /* There are really (bez.size-1)/3 sets of control */
	    /* points, with the first set containing teh first 4 */
	    /* points, and then every other set starting with the */
	    /* last point from the previous set and then the next 3 */
	    for (k = 1; k < bez.size; k += 3) {
		PROTECT(curCP = NEW_OBJECT(cpClass));
		PROTECT(pntSet = allocVector(VECSXP, 4));
		for (l = -1; l < 3; l++) {
		    PROTECT(curXY = NEW_OBJECT(xyClass));
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


