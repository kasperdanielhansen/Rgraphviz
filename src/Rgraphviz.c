#include "common.h"
#include "circle.h"
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

SEXP getListElement(SEXP list, char *str) {
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;

    for (i = 0; i < length(list); i++)
	if (strcmp(CHAR(STRING_ELT(names,i)), str) == 0) {
	    elmt = VECTOR_ELT(list, i);
	    break;
	}
    return(elmt);
}

static SEXP Rgraphviz_graph_type_tag;

#define CHECK_Rgraphviz_graph(s) do { \
     if (TYPEOF(s) != EXTPTRSXP || \
         R_ExternalPtrTag(s) != Rgraphviz_graph_type_tag) \
         error("bad graph reference"); \
} while (0)

SEXP Rgraphviz_init(void) {
    Rgraphviz_graph_type_tag = install("RGRAPH_TYPE_TAG");
    checkGraphvizVers();
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

SEXP Rgraphviz_agset(SEXP graph, SEXP attrs) {
    Agraph_t *g,*h;
    int i;
    Rboolean laidout;
    SEXP slotTmp, elmt, attrNames;

    /* !!!! Need to check the neames on attrs */

    laidout = (int)LOGICAL(GET_SLOT(graph, Rf_install("laidout")))[0];
    if (laidout == TRUE)
	error("graph is already laid out");
    slotTmp = GET_SLOT(graph, Rf_install("agraph"));
    CHECK_Rgraphviz_graph(slotTmp);
    g = R_ExternalPtrAddr(slotTmp);
    
    /* Currently only handling graph-wide attributes */
    PROTECT(elmt = getListElement(attrs, "graph"));
    /* Now elmt is a list of attributes to set */
    attrNames = getAttrib(elmt, R_NamesSymbol);
    for (i = 0; i < length(elmt); i++) {
	agraphattr(g, CHAR(STRING_ELT(attrNames,i)), STR(VECTOR_ELT(elmt,i)));
    }
    
    UNPROTECT(1);
    /* Now do node-wide ... NEED TO DO THIS BETTER */
    PROTECT(elmt = getListElement(attrs, "node"));
    attrNames = getAttrib(elmt, R_NamesSymbol);
    for (i = 0; i < length(elmt); i++) {
	agnodeattr(g, CHAR(STRING_ELT(attrNames,i)), STR(VECTOR_ELT(elmt,i)));
    }
    UNPROTECT(1);

    PROTECT(slotTmp = R_MakeExternalPtr(g,Rgraphviz_graph_type_tag,
					R_NilValue));
    R_RegisterCFinalizer(slotTmp, (R_CFinalizer_t)Rgraphviz_fin);
    SET_SLOT(graph, Rf_install("agraph"), slotTmp);

    
    slotTmp = GET_SLOT(graph, Rf_install("agraph"));
    h = R_ExternalPtrAddr(slotTmp);

    UNPROTECT(1);
    return(graph);
}

SEXP Rgraphviz_agread(SEXP filename) {
    Agraph_t *g;
    FILE *dotFile;

    dotFile = fopen(STR(filename),"r");
    if (dotFile == NULL) {
	error("Requested file does not exit");
    }
    aginit();
    g = agread(dotFile);
    return(buildRagraph(g));
}

SEXP Rgraphviz_agopen(SEXP name, SEXP kind, SEXP nodes, 
		      SEXP nodeLabels, SEXP from,
		      SEXP to, SEXP weights) {
    Agraph_t *g;
    Agnode_t *head, *tail, *tmp;
    Agedge_t *curEdge;
    int ag_k = 0;
    int i, curNode;
    char *tmpStr;
    
    if (!isInteger(kind))
	error("kind must be an integer value");
    else
	ag_k = INTEGER(kind)[0];

    if ((ag_k < 0)||(ag_k > 3))
	error("kind must be an integer value between 0 and 3");

    if (!isString(name))
	error("name must be a string");

    aginit();
    g = agopen(STR(name), ag_k);

    /* Set default attributes */
    g = setDefaultAttrs(g);

    /* Get the nodes created */
    for (i = 0; i < length(nodes); i++) {
/*	tmp = agnode(g, CHAR(STRING_ELT(nodes,i))); */
	tmp = agnode(g, CHAR(STRING_ELT(nodeLabels,i)));
/*	agset(tmp, "label", CHAR(STRING_ELT(nodeLabels,i)));
	printf("Setting label to |%s|\n", CHAR(STRING_ELT(nodeLabels,i)));
*/
    }

    /* now fill in the edges */
    for (i = 0; i < length(from); i++) {
	curNode = INTEGER(from)[i];
	tail = agfindnode(g, CHAR(STRING_ELT(nodeLabels,curNode-1)));
	if (tail == NULL)
	    error("Missing tail node");
	/* Get weights for these edges */
	curNode = INTEGER(to)[i];
	head = agfindnode(g,CHAR(STRING_ELT(nodeLabels,curNode-1)));
	if (head == NULL)
	    error("Missing head node");
	
	if (agfindedge(g, head, tail) == NULL) {
	    curEdge = agedge(g, tail, head);
	    curEdge->u.weight = INTEGER(weights)[i];
	}
	else {
	    if (ag_k == AGDIGRAPH) {
		curEdge = agfindedge(g, head, tail);
		agset(curEdge,"dir","both");
	    }
	}
    }

    return(buildRagraph(g));
}

SEXP Rgraphviz_doLayout(SEXP graph, SEXP layoutType) {
    Agraph_t *g;
    Rboolean laidout;
    SEXP slotTmp, nLayout, cPoints, bb;

    laidout = (int)LOGICAL(GET_SLOT(graph, Rf_install("laidout")))[0];
    if (laidout == FALSE) {
	slotTmp = GET_SLOT(graph, install("agraph"));
	CHECK_Rgraphviz_graph(slotTmp);
	g = R_ExternalPtrAddr(slotTmp);
	
	if (!isInteger(layoutType))
	    error("layoutType must be an integer value");
	else {
	    /* Note that we're using the standard dotneato */
	    /* layout commands for layouts and not the ones */
	    /* provided below.  This is a test */
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
	    default:
		error("Invalid layout type\n");
	    }
	}
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
	SET_SLOT(graph,Rf_install("AgEdge"), cPoints);
	SET_SLOT(graph,Rf_install("boundBox"), bb);
	UNPROTECT(4);
    }
    return(graph);
}

SEXP buildRagraph(Agraph_t *g) {
    SEXP graphRef, klass, obj;

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
    int nodes;
    int i,k,l,pntLstEl;
    int curEle = 0;

    epClass = MAKE_CLASS("AgEdge");
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
	    SET_SLOT(curEP, Rf_install("startArrow"),
		     R_scalarLogical(bez.sflag)); 
	    SET_SLOT(curEP, Rf_install("endArrow"),
		     R_scalarLogical(bez.eflag));
	    /* get the sp and ep */
	    PROTECT(curXY = NEW_OBJECT(xyClass));
	    SET_SLOT(curXY, Rf_install("x"),
		     R_scalarInteger(bez.sp.x));
	    SET_SLOT(curXY, Rf_install("y"),
		     R_scalarInteger(bez.sp.y));
	    SET_SLOT(curEP, Rf_install("sp"), curXY);
	    UNPROTECT(1);
	    PROTECT(curXY = NEW_OBJECT(xyClass));
	    SET_SLOT(curXY, Rf_install("x"),
		     R_scalarInteger(bez.ep.x));
	    SET_SLOT(curXY, Rf_install("y"),
		     R_scalarInteger(bez.ep.y));
	    SET_SLOT(curEP, Rf_install("ep"), curXY);
	    UNPROTECT(1);	    
	
	    SET_ELEMENT(outList, curEle++, curEP);
	    UNPROTECT(2);
	    edge = agnxtout(g, edge);
	}
	node = agnxtnode(g, node);
    }
    UNPROTECT(1);

    return(outList);
}

void checkGraphvizVers(void) {
    char *out;
    int ret;

    /* Compare the current version, Info[2] w/ MINGRAPHVIZVER */
    /* Throw an error if it isn't at least equal to min ver */
    ret = strcmp(Info[1], MINGRAPHVIZVER);
    if (ret < 0) {
	out = (char *)R_alloc(100+strlen(MINGRAPHVIZVER),sizeof(char));
	sprintf(out,
		"Your graphviz installation is out of date, please use version %s or newer", MINGRAPHVIZVER);
	error(out);
    }
}

Agraph_t *setDefaultAttrs(Agraph_t *g) {
    /* While attributes have default values already,  */
    /* if we want to dynamically set them, we need */
    /* to have defined defaults manually */

    /* Note that not all defaults are exactly as in normal graphviz */

    /*** GRAPH ATTRS ***/
    /* Neato overlap type */
    agraphattr(g, "overlap", "");
    agraphattr(g, "splines", "true");
    agraphattr(g, "model", "");

    /*** NODE ATTRS ***/
/*    agnodeattr(g, "height", "0.71");
    agnodeattr(g, "width", "2.83");
    agnodeattr(g, "fixedsize", "true");
*/
    /*** EDGE ATTRS ***/
    /* Arrow direction */
    if (AG_IS_DIRECTED(g)) 
	agedgeattr(g, "dir", "forward");
    else
	agedgeattr(g, "dir", "none");
    agedgeattr(g, "weight", "1.0");


    /* Neato spline type */

    return(g);
}

/***************************************************************/
/* These layout commands are probably unecessary in the future */
/* But keeping them around for now just in case                */
/***************************************************************/

Agraph_t *dotLayout(Agraph_t *g) {
    dot_init_graph(g);
    dot_rank(g);
    dot_mincross(g);
    dot_position(g);
    dot_sameports(g);
    dot_splines(g);
    dotneato_postprocess(g, dot_nodesize);

    return(g);
}

Agraph_t *neatoLayout(Agraph_t *g) {
    int nG;
    char *p;
    attrsym_t* sym;

    sym = agfindattr(g,"rankdir");
    if (sym)
	agxset(g, sym->index, "");

    agset(g, "overlap", "scale");
    agset(g, "splines", "true");

    neato_init_graph(g);
    nG = scan_graph(g);
    p = agget(g,"model");
    if (p && (streq(p,"circuit")))
	circuit_model(g,nG);
    else
	shortest_path(g, nG);
    initial_positions(g, nG);
    diffeq_model(g, nG);
    solve_model(g, nG);
    final_energy(g, nG);
    adjustNodes(g);
    spline_edges(g);
    dotneato_postprocess(g, dot_nodesize);
    return(g);
}

Agraph_t *twopiLayout(Agraph_t *g) {
    Agnode_t* ctr;

    twopi_init_graph(g); 
    ctr = agfstnode(g);   
    circleLayout(g,ctr);
    adjustNodes(g);
    spline_edges(g);
    dotneato_postprocess(g,twopi_nodesize);

    return(g);
}
