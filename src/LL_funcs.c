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

// TODO:
// o. validate ptrs to graph/node/edge
// -- getAttrs
// o. handle non-exist attr
// o. handle given default attr
// -- setAttrs
// o. check attr default/val
// o. ??? validate attr default/val, e.g., red for color, 123 not for shape ???

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

     char* attrs[] = { 
     		"bgcolor",
     		"fontcolor",
     		"ratio",
     		"overlap",
     		"splines",

     		"rank",
     		"size",
     		"rankdir",	// dot only
		NULL
		};
     int nattr = 0;
     while ( attrs[nattr] ) nattr++;

     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     SEXP ans, at_name, at_val;
     PROTECT(ans = allocMatrix(STRSXP, nattr, 2));

     Agsym_t* sym;
     char* val;
     int i = 0, ii = 0;
     for ( i = 0, ii = 0; i < nattr; i++, ii++ )
     {
        sym = agfindattr(g, attrs[i]);
        val = sym? sym->value : NULL;
        if ( !val ) val = "ATTR_NOT_DEFINED";

        SET_STRING_ELT(ans, ii, mkChar(attrs[i]));
        SET_STRING_ELT(ans, nattr+ii, mkChar(val));

#if DEBUG
        if ( sym )
        printf(" attr name: %s \t attr val: %s \n", sym->name, sym->value); 
#endif
     }
     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_getAttrsGraph(SEXP graph, SEXP attrname)
{
     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     char *val = agget(g, STR(attrname));

     if ( !val ) // no such attr
     {
         val = "default graph attr val 1";
     }
     else if ( !strlen(val) ) // attr defined but use default
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

     // 0 for success, -1 otherwise
     int r = agsafeset(g, STR(attrname), STR(attrval), STR(default_val));

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

     char* attrs[] = { 
     		"shape",
     		"fixedsize",
     		"fillcolor",
     		"label",
     		"color",

     		"fontcolor",
     		"fontsize",
     		"height",
     		"width",
		NULL
		};
     int nattr = 0;
     while ( attrs[nattr] ) nattr++;

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
        sym = agfindattr(n, attrs[i]);
        val = sym? sym->value : NULL;
        if ( !val ) val = "ATTR_NOT_DEFINED";

        SET_STRING_ELT(ans, ii, mkChar(attrs[i]));
        SET_STRING_ELT(ans, nattr+ii, mkChar(val));

#if DEBUG
        if ( sym )
        printf(" attr name: %s \t attr val: %s \n", sym->name, sym->value); 
#endif
     }

     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_getAttrsNode(SEXP graph, SEXP node, SEXP attrname)
{
     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     Agnode_t *n = agfindnode(g, STR(node));
     if ( !n ) return(R_NilValue);

     char *val = agget(n, STR(attrname));

     if ( !val ) // no such attr
     {
         val = "default node attr val 1";
     }
     else if ( !strlen(val) ) // attr defined but use default
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

     int r = agsafeset(n, STR(attrname), STR(attrval), STR(default_val));

     SEXP ans;
     PROTECT(ans = NEW_LOGICAL(1));
     LOGICAL(ans)[0] = r? FALSE : TRUE;
     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_getDefAttrsEdge(SEXP graph)
{
#if DEBUG 
     // entries that are set are the same set of attrs as listed below
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

     char* attrs[] = {
     		"color",
     		"dir",
     		"weight",
     		"label",
     		"fontcolor",
		
     		"arrowhead",
     		"arrowtail",
     		"fontsize",
     		"labelfontsize",
     		"arrowsize",
		
     		"headport",
     		"layer",
     		"style",
     		"minlen",	// dot only
		"len",		// neato only
		NULL
		};
     int nattr = 0;
     while ( attrs[nattr] ) nattr++;

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
        sym = agfindattr(e, attrs[i]);
        if ( sym ) val = sym? sym->value : NULL;
        if ( !val ) val = "ATTR_NOT_DEFINED";

        SET_STRING_ELT(ans, ii, mkChar(attrs[i]));
        SET_STRING_ELT(ans, nattr+ii, mkChar(val));

#if DEBUG
        if ( sym )
        printf(" attr name: %s \t attr val: %s \n", sym->name, sym->value); 
#endif
     }
     UNPROTECT(1);
     return(ans);
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

     if ( !val ) // no such attr
     {
         val = "default edge attr val 1";
     }
     else if ( !strlen(val) ) // attr defined but use default
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

     int r = agsafeset(e, STR(attrname), STR(attrval), STR(default_val));

     SEXP ans;
     PROTECT(ans = NEW_LOGICAL(1));
     LOGICAL(ans)[0] = r? FALSE : TRUE;
     UNPROTECT(1);
     return(ans);
}

SEXP Rgraphviz_toFile(SEXP graph, SEXP layoutType, SEXP filename, SEXP filetype)
{
     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);

     int i1 = gvLayout(gvc, g, STR(layoutType));

     int i2 = gvRenderFilename(gvc, g, STR(filetype), STR(filename));

     int i3 = gvFreeLayout(gvc, g);

     return(R_NilValue);
}

/*
SEXP Rgraphviz_drawNode(SEXP graph, SEXP node)
{
     Agraph_t *g = getAgraphPtr(graph);
     if ( !g ) return(R_NilValue);
     
     Agnode_t *np = agfindnode(g, STR(node));
     if ( !np ) return(R_NilValue);

     //ND_width(np);
     //ND_height(np);
     //ND_coord_i(np);
     if ( strcmp(ND_shape(np)->name, "box") == 0 ||
	  strcmp(ND_shape(np)->name, "rect") == 0 ||
	  strcmp(ND_shape(np)->name, "rectangle") == 0 ||
	  strcmp(ND_shape(np)->name, "polygon") == 0 // ||
	) 
     {
	Nd_shape_info(np)->vertice 
     }
     else if ( strcmp(ND_shape(np)->name, "circle") == 0 ) 
     {
     }
     else if ( strcmp(ND_shape(np)->name, "ellipse") == 0 ) 
     {
     }
     else if ( strcmp(ND_shape(np)->name, "plaintext") == 0 ) 
     {
     }

     return(R_NilValue);
}
*/

/*
default attrs for cluster:

bgcolor
color
rank

*/

