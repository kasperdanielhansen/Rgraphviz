#include "common.h"

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

SEXP Rgraphviz_doLayout(SEXP graph) {
    Agraph_t *g;

    CHECK_Rgraphviz_graph(graph);
    g = R_ExternalPtrAddr(graph);

    graph_init(g);    
    g->u.drawing->engine = DOT;
    dot_init_node_edge(g);
    dot_rank(g);
    dot_mincross(g);
    dot_position(g);
    dot_sameports(g);
    dot_splines(g);
    dotneato_postprocess(g, dot_nodesize);

    Rprintf("Layout complete.\n");
    graph = R_MakeExternalPtr(g, Rgraphviz_graph_type_tag, R_NilValue);
    R_RegisterCFinalizer(graph, (R_CFinalizer_t)Rgraphviz_fin);
    return(graph);
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

    return(R_NilValue);
}

SEXP Rgraphviz_agopen(SEXP name, SEXP kind, SEXP nelist,
		   SEXP weightList) {
    Agraph_t *g;
    Agnode_t *head, *tail;
    Agedge_t *curEdge;
    SEXP graphRef, elmt, names, curWeight, weightVals, weightNames;
    int ag_k, i,j;

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
    for (i = 0; i < length(names); i++)
	agnode(g, CHAR(STRING_ELT(names,i)));
    /* now fill in the edges */
    for (i = 0; i < length(nelist); i++) {
	PROTECT(elmt = VECTOR_ELT(nelist, i));
	head = agfindnode(g, CHAR(STRING_ELT(names,i)));
	/* Get weights for these edges */
	PROTECT(curWeight = VECTOR_ELT(weightList,i));
	PROTECT(weightNames = getAttrib(curWeight, R_NamesSymbol));
	PROTECT(weightVals = AS_INTEGER(curWeight));
	for (j = 0; j < length(elmt); j++) {
	    tail = agfindnode(g,CHAR(STRING_ELT(elmt,j)));
	    curEdge = agedge(g, tail, head);
	    curEdge->u.weight = INTEGER(curWeight)[j];
	}
	UNPROTECT(4);
    }
    UNPROTECT(1);
    graphRef = R_MakeExternalPtr(g,Rgraphviz_graph_type_tag,
				 R_NilValue);
    R_RegisterCFinalizer(graphRef, (R_CFinalizer_t)Rgraphviz_fin);

    return(graphRef);
}

