#include "common.h"
#include "util.h"
#include <stdio.h>

static inline Agraph_t *getAgraphPtr(SEXP graph)
{
     SEXP slotTmp = GET_SLOT(graph, Rf_install("agraph"));
     CHECK_Rgraphviz_graph(slotTmp);
     Agraph_t *g = R_ExternalPtrAddr(slotTmp);
     return g;
}

/*
 * TODO:
 * o. validate ptrs to graph/node/edge
 * -- getAttrs
 * o. handle non-exist attr
 * o. handle given default attr
 * -- setAttrs
 * o. check attr default/val
 * o. ??? validate attr default/val, e.g., red for color, 123 not for shape ???
 * o. call "agclose(g)" somewhere...
*/

static const Rgattr_t def_graph_attrs[] = { 
     		{"bgcolor",	"transparent"},
     		{"fontcolor",	"black"},
     		{"ratio",	"fill"},
     		{"overlap",	""},
     		{"splines",	"TRUE"},

     		{"rank",	"same"},
     		{"size",	"6.99, 6.99"},
     		{"rankdir",	"TB"},	/* dot only */

		{NULL,		NULL}
		};

static const Rgattr_t def_node_attrs[] = { 
     		{"shape",	"circle"},
     		{"fixedsize",	"TRUE"},
     		{"fillcolor",	"transparent"},
     		{"label",	""},
     		{"color",	"black"},

     		{"fontcolor",	"black"},
     		{"fontsize",	"14"},
     		{"height",	"0.5"},
     		{"width",	"0.75"},

		{NULL,		NULL}
		};

static const Rgattr_t def_edge_attrs[] = {
     		{"color",	"black"},
     		{"dir",		"both"},
     		{"weight",	"1.0"},
     		{"label",	""},
     		{"fontcolor",	"black"},
		
     		{"arrowhead",	"none"},
     		{"arrowtail",	"none"},
     		{"fontsize",	"14"},
     		{"labelfontsize","11"},
     		{"arrowsize",	"1"},
		
     		{"headport",	"center"},
     		{"layer",	""},
     		{"style",	"solid"},
     		{"minlen",	"1"},	/* dot only */
		{"len",		"1.0"},	/* neato only */

		{NULL, NULL}
		};


#ifdef GRAPHVIZ_2_10_TO_MORE


SEXP Rgraphviz_getDefAttrsGraph(SEXP graph)
{
#if DEBUG
     attrsym_t* defattrs[] = {
     		G_activepencolor,
     		G_activefillcolor,
     		G_selectedpencolor,
     		G_selectedfillcolor,
     		G_visitedpencolor,

     		G_visitedfillcolor,
     		G_deletedpencolor,
     		G_deletedfillcolor,
     		G_peripheries,
		NULL
		};
     int nnattr = 0;
     while ( defattrs[nnattr] ) nnattr++;

     for ( int jj = 0; jj < nnattr; jj++ )
     {
	if ( defattrs[jj] && defattrs[jj]->name && defattrs[jj]->value )
	printf("%d : %s = %s \n", jj, defattrs[jj]->name, defattrs[jj]->value);
     }

#endif

     int nattr = 0;
     while ( def_graph_attrs[nattr].name ) nattr++;

     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     SEXP ans;
     PROTECT(ans = allocMatrix(STRSXP, nattr, 2));

     Agsym_t* sym;
     char* val;
     int i = 0, ii = 0;
     for ( i = 0, ii = 0; i < nattr; i++, ii++ )
     {
        sym = agfindattr(g, def_graph_attrs[i].name);
        val = sym? sym->value : NULL;
        if ( !val ) val = "ATTR_NOT_DEFINED";

        SET_STRING_ELT(ans, ii, mkChar(def_graph_attrs[i].name));
        SET_STRING_ELT(ans, nattr+ii, mkChar(val));

#if DEBUG
        if ( sym )
        printf(" attr name: %s \t attr val: %s \n", sym->name, sym->value); 
#endif
     }
     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_setDefAttrsGraph()
{
     int nattr = 0;
     int i;
     while ( def_graph_attrs[nattr].name ) nattr++;

     for ( i = 0; i < nattr; i++ )
        agraphattr(NULL, def_graph_attrs[i].name, def_graph_attrs[i].value);

     return(R_NilValue);
}

SEXP Rgraphviz_getAttrsGraph(SEXP graph, SEXP attrname)
{
     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     char *val = agget(g, STR(attrname));

     if ( !val ) /* no such attr */
     {
         val = "default graph attr val 1";
     }
     else if ( !strlen(val) ) /* attr defined but use default */
     {
         val = "default graph attr val 2";
     }

     SEXP ans;
     PROTECT(ans = allocVector(STRSXP, 1));
     SET_STRING_ELT(ans, 0, mkChar(val));
     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_setAttrsGraph(SEXP graph, 
		SEXP attrname, SEXP attrval, SEXP default_val)
{
     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     /* 0 for success, -1 otherwise */
/*
#if OLDER_THAN_2_8 
*/
     Agsym_t* a = agfindattr(g, STR(attrname));
     if ( !a ) a = agraphattr(g->root, STR(attrname), STR(default_val));
     int r = agset(g, STR(attrname), STR(attrval));
/*
#else
     int r = agsafeset(g, STR(attrname), STR(attrval), STR(default_val));
#endif 
*/

     SEXP ans;
     PROTECT(ans = NEW_LOGICAL(1));
     LOGICAL(ans)[0] = r? FALSE : TRUE;
     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_getDefAttrsNode(SEXP graph)
{
#if DEBUG
     attrsym_t* defattrs[] = {
        	N_height,
		N_width,
		N_shape,
		N_color,
		N_fillcolor,
	
        	N_activepencolor,
		N_activefillcolor,
        	N_selectedpencolor,
		N_selectedfillcolor,
        	N_visitedpencolor,
	
		N_visitedfillcolor,
        	N_deletedpencolor,
		N_deletedfillcolor,
        	N_fontsize,
		N_fontname,
	
		N_fontcolor,
        	N_label,
		N_nojustify,
		N_style,
		N_showboxes,
	
        	N_sides,
		N_peripheries,
		N_orientation,
        	N_skew,
		N_distortion,
	
		N_fixed,
		N_layer,
        	N_group,
		N_comment,
		N_vertices,
	
		N_z,
		NULL
		};
     int nnattr = 0;
     while ( defattrs[nnattr] ) nnattr++;

     for ( int jj = 0; jj < nnattr; jj++ )
     {
	if ( defattrs[jj] && defattrs[jj]->name && defattrs[jj]->value )
	printf("%d : %s = %s \n", jj, defattrs[jj]->name, defattrs[jj]->value);
     }

#endif

     int nattr = 0;
     while ( def_node_attrs[nattr].name ) nattr++;

     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     Agnode_t *n = g->proto->n;

     SEXP ans;
     PROTECT(ans = allocMatrix(STRSXP, nattr, 2));

     Agsym_t* sym;
     char* val;
     int i = 0, ii = 0;
     for ( i = 0, ii = 0; i < nattr; i++, ii++ )
     {
        sym = agfindattr(n, def_node_attrs[i].name);
        val = sym? sym->value : NULL;
        if ( !val ) val = "ATTR_NOT_DEFINED";

        SET_STRING_ELT(ans, ii, mkChar(def_node_attrs[i].name));
        SET_STRING_ELT(ans, nattr+ii, mkChar(val));

#if DEBUG
        if ( sym )
        printf(" attr name: %s \t attr val: %s \n", sym->name, sym->value); 
#endif
     }

     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_setDefAttrsNode()
{
     int nattr = 0;
     while ( def_node_attrs[nattr].name ) nattr++;

     int i;
     for ( i = 0; i < nattr; i++ )
	agnodeattr(NULL, def_node_attrs[i].name, def_node_attrs[i].value);

     return(R_NilValue);
}

SEXP Rgraphviz_getAttrsNode(SEXP graph, SEXP node, SEXP attrname)
{
     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     Agnode_t *n = agfindnode(g, STR(node));
     if ( !n ) return(R_NilValue);

     char *val = agget(n, STR(attrname));

     if ( !val ) /* no such attr */
     {
         val = "default node attr val 1";
     }
     else if ( !strlen(val) ) /* attr defined but use default */
     {
         val = "default node attr val 2";
     }

     SEXP ans;
     PROTECT(ans = allocVector(STRSXP, 1));
     SET_STRING_ELT(ans, 0, mkChar(val));
     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_setAttrsNode(SEXP graph, SEXP node, 
		SEXP attrname, SEXP attrval, SEXP default_val)
{
     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     Agnode_t *n = agfindnode(g, STR(node));
     if ( !n ) return(R_NilValue);

/*
#if OLDER_THAN_2_8
*/
     Agsym_t* a = agfindattr(n, STR(attrname));
     if ( !a ) a = agnodeattr(g, STR(attrname), STR(default_val));
     int r = agset(n, STR(attrname), STR(attrval));
/*
#else
     int r = agsafeset(n, STR(attrname), STR(attrval), STR(default_val));
#endif 
*/

     SEXP ans;
     PROTECT(ans = NEW_LOGICAL(1));
     LOGICAL(ans)[0] = r? FALSE : TRUE;
     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_getDefAttrsEdge(SEXP graph)
{
#if DEBUG 
     /* entries that are set are the same set of attrs as listed below */
     attrsym_t* defattrs[] = {
        	E_weight, 
		E_minlen, 
		E_color,			
        	E_activepencolor, 
		E_activefillcolor,	
	
        	E_selectedpencolor, 
		E_selectedfillcolor,
        	E_visitedpencolor, 
		E_visitedfillcolor,
        	E_deletedpencolor, 
	
		E_deletedfillcolor,
        	E_fontsize, 
		E_fontname, 
		E_fontcolor,
        	E_label, 
	
		E_dir, 
		E_style, 
		E_decorate,
        	E_showboxes, 
		E_arrowsz, 
	
		E_constr, 
		E_layer,
        	E_comment, 
		E_label_float,
        	E_samehead, 
	
		E_sametail,	
        	E_arrowhead, 
		E_arrowtail,
        	E_headlabel, 
		E_taillabel,
	
        	E_labelfontsize, 
		E_labelfontname, 
		E_labelfontcolor,
        	E_labeldistance, 
		E_labelangle,
	
        	E_tailclip, 
		E_headclip,
		NULL
		};
     int nnattr = 0;
     while ( defattrs[nnattr] ) nnattr++;

     for ( int jj = 0; jj < nnattr; jj++ )
     {
	if ( defattrs[jj] && defattrs[jj]->name && defattrs[jj]->value )
	printf("%d : %s = %s \n", jj, defattrs[jj]->name, defattrs[jj]->value);
     }

#endif

     int nattr = 0;
     while ( def_edge_attrs[nattr].name ) nattr++;

     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     Agedge_t *e = g->proto->e;

     SEXP ans;
     PROTECT(ans = allocMatrix(STRSXP, nattr, 2));

     Agsym_t* sym;
     char* val;
     int i = 0, ii = 0;
     for ( i = 0, ii = 0; i < nattr; i++, ii++ )
     {
        sym = agfindattr(e, def_edge_attrs[i].name);
        val = sym? sym->value : NULL;
        if ( !val ) val = "ATTR_NOT_DEFINED";

        SET_STRING_ELT(ans, ii, mkChar(def_edge_attrs[i].name));
        SET_STRING_ELT(ans, nattr+ii, mkChar(val));

#if DEBUG
        if ( sym )
        printf(" attr name: %s \t attr val: %s \n", sym->name, sym->value); 
#endif
     }
     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_setDefAttrsEdge()
{
     int nattr = 0;
     while ( def_edge_attrs[nattr].name ) nattr++;

     int i;
     for ( i = 0; i < nattr; i++ )
	agedgeattr(NULL, def_edge_attrs[i].name, def_edge_attrs[i].value);

     return(R_NilValue);
}

SEXP Rgraphviz_getAttrsEdge(SEXP graph, SEXP from, SEXP to, SEXP attrname)
{
     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     Agnode_t *u = agfindnode(g, STR(from));
     Agnode_t *v = agfindnode(g, STR(to));
     if ( !u || !v ) return(R_NilValue);

     Agedge_t *e = agfindedge(g, u, v);
     if ( !e ) return(R_NilValue);

     char *val = agget(e, STR(attrname));

     if ( !val ) /* no such attr */
     {
         val = "default edge attr val 1";
     }
     else if ( !strlen(val) ) /* attr defined but use default */
     {
         val = "default edge attr val 2";
     }

     SEXP ans;
     PROTECT(ans = allocVector(STRSXP, 1));
     SET_STRING_ELT(ans, 0, mkChar(val));
     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_setAttrsEdge(SEXP graph, SEXP from, SEXP to, 
		SEXP attrname, SEXP attrval, SEXP default_val)
{
     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     Agnode_t *u = agfindnode(g, STR(from));
     Agnode_t *v = agfindnode(g, STR(to));
     if ( !u || !v ) return(R_NilValue);

     Agedge_t *e = agfindedge(g, u, v);
     if ( !e ) return(R_NilValue);

/*
#if OLDER_THAN_2_8
*/
     Agsym_t* a = agfindattr(e, STR(attrname));
     if ( !a ) a = agedgeattr(g, STR(attrname), STR(default_val));
     int r= agset(e, STR(attrname), STR(attrval));
/*
#else
     int r = agsafeset(e, STR(attrname), STR(attrval), STR(default_val));
#endif
*/

     SEXP ans;
     PROTECT(ans = NEW_LOGICAL(1));
     LOGICAL(ans)[0] = r? FALSE : TRUE;
     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_toFile(SEXP graph, SEXP layoutType, SEXP filename, SEXP filetype)
{
#ifndef GRAPHVIZ_2_2_TO_2_3
     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     int i1 = gvLayout(gvc, g, STR(layoutType));

     int i2 = gvRenderFilename(gvc, g, STR(filetype), STR(filename));

     int i3 = gvFreeLayout(gvc, g);
#endif

     return(R_NilValue);
}

/*
 * g: graphNEL
 * nodes = nodes(g), 	strings
 * edges_from = edgeMatrix(g)["from",], edges_to = edgeMatrix(g)["to", ],  ints
 * nsubG = no. of subgraphs
 * subGIndex = subgraph-index for nodes, ints
*/
SEXP LLagopen(SEXP name, SEXP kind, 
	      SEXP nodes, SEXP edges_from, SEXP edges_to, 
	      SEXP nsubG, SEXP subGIndex, SEXP recipEdges)
{
    Agraph_t *g, *tmpGraph;
    Agraph_t **sgs;
    Agnode_t *head, *tail, *curNode;
    Agedge_t *curEdge;
    SEXP curPN, curPE, curSubG, curSubGEle;

    char subGName[256];
    int ag_k = 0;
    int nsg = INTEGER(nsubG)[0];
    int i,j;
    int whichSubG;

    if ( length(edges_from) != length(edges_to) )
       error("length of edges_from must be equal to length of edges_to");

    if ( length(nodes) != length(subGIndex) )
       error("length of nodes must be equal to length of subGIndex");

    if (!isString(name)) error("name must be a string");

    if (!isInteger(kind)) error("kind must be an integer value");     

    ag_k = INTEGER(kind)[0]; 
    if ((ag_k < 0)||(ag_k > 3))
        error("kind must be an integer value between 0 and 3");

    aginit();
    g = agopen(STR(name), ag_k);

    /* create subgraphs */
    sgs = (Agraph_t **)R_alloc(nsg, sizeof(Agraph_t *));
    if ( nsg > 0 && !sgs ) 
	error("Out of memory while allocating subgraphs");

    for (i = 0; i < nsg; i++) {
/*
            curSubG = VECTOR_ELT(subGs, i);

            // First see if this is a cluster or not 
            curSubGEle = getListElement(curSubG, "cluster");
            if ( curSubGEle == R_NilValue || LOGICAL(curSubGEle)[0] )
                sprintf(subGName, "%s%d", "cluster_", i);
            else
*/
                sprintf(subGName, "%d", i);

/*	    printf(" subgraph %d is named: %s \n", i, subGName); */
            sgs[i] = agsubg(g, subGName);
     }

#if DEBUG
    printf(" nodes: ");
    for (i = 0; i < length(nodes); i++) {
	printf("%s ", CHAR(STRING_ELT(nodes, i)));
    }
    printf("\n");
    printf(" edges: ");
    for (i = 0; i < length(edges_from); i++) {
	printf("%d - %d     %s - %s \n", 
		INTEGER(edges_from)[i], 
		INTEGER(edges_to)[i],
		CHAR(STRING_ELT(nodes, INTEGER(edges_from)[i]-1)),
		CHAR(STRING_ELT(nodes, INTEGER(edges_to)[i]-1)));
    }
#endif

    /* create nodes */
    for (i = 0; i < length(nodes); i++) {
        whichSubG = INTEGER(subGIndex)[i];
        tmpGraph = whichSubG > 0 ? sgs[whichSubG-1] : g;

        curNode = agnode(tmpGraph, CHAR(STRING_ELT(nodes, i)));

/*	printf(" node %d in subgraph %d \n", i, whichSubG); */
    }

    /* create the edges */
    char* node_f; char* node_t;
    for (i = 0; i < length(edges_from); i++) {
	node_f = CHAR(STRING_ELT(nodes, INTEGER(edges_from)[i]-1));
	node_t = CHAR(STRING_ELT(nodes, INTEGER(edges_to)[i]-1));

        tail = agfindnode(g, node_f);
        if ( !tail ) error("Missing tail node");

        head = agfindnode(g, node_t);
        if ( !head ) error("Missing head node");

        whichSubG = INTEGER(subGIndex)[INTEGER(edges_from)[i]-1]; 
        tmpGraph = whichSubG > 0 ? sgs[whichSubG-1] : g;

        curEdge = agedge(tmpGraph, tail, head);
    }

    return(buildRagraph(g));

}

#else

SEXP Rgraphviz_getDefAttrsGraph(SEXP graph)
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP Rgraphviz_setDefAttrsGraph()
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP Rgraphviz_getAttrsGraph(SEXP graph, SEXP attrname)
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP Rgraphviz_setAttrsGraph(SEXP graph, 
		SEXP attrname, SEXP attrval, SEXP default_val)
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP Rgraphviz_getDefAttrsNode(SEXP graph)
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP Rgraphviz_setDefAttrsNode()
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP Rgraphviz_getAttrsNode(SEXP graph, SEXP node, SEXP attrname)
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP Rgraphviz_setAttrsNode(SEXP graph, SEXP node, 
		SEXP attrname, SEXP attrval, SEXP default_val)
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP Rgraphviz_setDefAttrsEdge()
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP Rgraphviz_getAttrsEdge(SEXP graph, SEXP from, SEXP to, SEXP attrname)
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP Rgraphviz_setAttrsEdge(SEXP graph, SEXP from, SEXP to, 
		SEXP attrname, SEXP attrval, SEXP default_val)
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP Rgraphviz_toFile(SEXP graph, SEXP layoutType, SEXP filename, SEXP filetype)
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

SEXP LLagopen(SEXP name, SEXP kind, 
	      SEXP nodes, SEXP edges_from, SEXP edges_to, 
	      SEXP nsubG, SEXP subGIndex, SEXP recipEdges) 
{
    warning("This function is not supported by your current Graphviz installation.\n");
    return(R_NilValue);
}

#endif
