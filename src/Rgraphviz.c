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


SEXP Rgraphviz_agopen(SEXP name, SEXP kind, SEXP nelist,
		      SEXP weightList) {
    Agraph_t *g;
    Agnode_t *head, *tail;
    Agedge_t *curEdge;
    SEXP graphRef, elmt, names, curWeight, weightVals, weightNames,
	obj, klass;
    int ag_k = 0;
    int i,j;

    if (!isInteger(kind))
	error("kind must be an integer value");
    else
	ag_k = INTEGER(kind)[0];

    if (!isString(name))
	error("name must be a string");

    if (!isNewList(nelist))
	error("nelist must be a list");

    if (!isNewList(weightList))
	error("weightListmust be a list");

    if (length(weightList) != length(nelist))
	error("Weights must be the same length as nelist");

    aginit();
    g = agopen(STR(name), ag_k);

    PROTECT(names = getAttrib(nelist, R_NamesSymbol));

    /* Get the nodes created */
    for (i = 0; i < length(names); i++) {
	agnode(g, CHAR(STRING_ELT(names,i)));
    }

    /* now fill in the edges */
    for (i = 0; i < length(nelist); i++) {
	PROTECT(elmt = AS_CHARACTER(VECTOR_ELT(nelist, i)));
	head = agfindnode(g, CHAR(STRING_ELT(names,i)));
	if (head == NULL)
	    error("Missing head node");
	/* Get weights for these edges */
	curWeight = VECTOR_ELT(weightList,i);
	PROTECT(weightNames = getAttrib(curWeight, R_NamesSymbol));
	PROTECT(weightVals = AS_INTEGER(curWeight));
	for (j = 0; j < length(elmt); j++) {
	    tail = agfindnode(g,CHAR(STRING_ELT(elmt,j)));
	    if (tail == NULL)
		error("Missing tail node");
	    if (agfindedge(g,head,tail) == NULL) {  
		curEdge = agedge(g, tail, head);
		curEdge->u.weight = INTEGER(curWeight)[j];
	    }  
	}
	UNPROTECT(3);
    }
    UNPROTECT(1);

    PROTECT(graphRef = R_MakeExternalPtr(g,Rgraphviz_graph_type_tag,
				 R_NilValue));
    R_RegisterCFinalizer(graphRef, (R_CFinalizer_t)Rgraphviz_fin);

    klass = MAKE_CLASS("Ragraph");
    /*XX  the call to duplicate is needed until 1.6.2 is released
      because of a bug in the NEW() mechanism in < 1.6.2! */
    PROTECT(obj = duplicate(NEW_OBJECT(klass)));
    
    SET_SLOT(obj, Rf_install("agraph"), graphRef);
    SET_SLOT(obj, Rf_install("laidout"), R_scalarLogical(FALSE));

    UNPROTECT(2);

    return(obj);
}

SEXP Rgraphviz_doDotLayout(SEXP graph) {
    Agraph_t *g;
    Rboolean laidout;
    SEXP slotTmp, pos;

    laidout = (int)LOGICAL(GET_SLOT(graph, Rf_install("laidout")))[0];
    if (laidout == FALSE) {
	slotTmp = GET_SLOT(graph, install("agraph"));
	CHECK_Rgraphviz_graph(slotTmp);
	g = R_ExternalPtrAddr(slotTmp);
	
	g = dotLayout(g);
	PROTECT(pos = getNodeLocs(g));
	
	PROTECT(slotTmp = R_MakeExternalPtr(g,Rgraphviz_graph_type_tag,
					     R_NilValue));
	R_RegisterCFinalizer(slotTmp, (R_CFinalizer_t)Rgraphviz_fin);
	SET_SLOT(graph, Rf_install("agraph"), slotTmp);
	SET_SLOT(graph,Rf_install("nodeLocs"),pos);
	SET_SLOT(graph,Rf_install("laidout"), R_scalarLogical(TRUE));
	UNPROTECT(2);
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


