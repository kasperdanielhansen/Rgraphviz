#ifndef __COMMON_H_INCLUDED
#define __COMMON_H_INCLUDED

extern char *Info[] = {
	    "Rgraphviz",              /* Program */
	    "1.0",            /* Version */
	    "10/21/02"                /* Build Date */
};

#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/RConverters.h>
#ifndef WIN32
#include <unistd.h>
#endif

#include <math.h>
#include <gvconfig.h>
#include <render.h>
#include <graph.h>
#include <dotprocs.h>
#include <neatoprocs.h>
/*#include <adjust.h>
*/
#define AGRAPH_T(x) ((agraph_t *)DATAPTR(x))
#define STR(SE) CHAR(STRING_ELT(SE,0))

SEXP Rgraphviz_init(void);
SEXP Rgraphviz_fin(SEXP);
SEXP Rgraphviz_graph2ps(SEXP, SEXP);
SEXP Rgraphviz_doDotLayout(SEXP);
SEXP Rgraphviz_agopen(SEXP, SEXP, SEXP, SEXP);
SEXP getNodeLocs(Agraph_t *);
Agraph_t *dotLayout(Agraph_t *);

#endif
