#ifndef __COMMON_H_INCLUDED
#define __COMMON_H_INCLUDED

extern char *Info[] = {
	    "Rgraphviz",              /* Program */
	    "1.0",            /* Version */
	    "10/21/02"                /* Build Date */
};

#define DOTLAYOUT 0
#define NEATOLAYOUT 1
#define TWOPILAYOUT 2

#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/RConverters.h>
#ifndef WIN32
#include <unistd.h>
#endif

#include <math.h>
/*#include <gvconfig.h>*/
#include <render.h>
#include <graph.h>
#include <dotprocs.h>
#include <neatoprocs.h>
#include <adjust.h>


#define AGRAPH_T(x) ((agraph_t *)DATAPTR(x))
#define STR(SE) CHAR(STRING_ELT(SE,0))

SEXP R_scalarReal(double);
SEXP R_scalarInteger(int);
SEXP R_scalarLogical(Rboolean);
SEXP getListElement(SEXP, char*);

SEXP Rgraphviz_init(void);
SEXP Rgraphviz_fin(SEXP);
SEXP Rgraphviz_doLayout(SEXP, SEXP);
SEXP getBoundBox(Agraph_t *);
SEXP getEdgeLocs(Agraph_t *,int);
SEXP Rgraphviz_agopen(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP getNodeLayouts(Agraph_t *);
Agraph_t *dotLayout(Agraph_t *);
Agraph_t *neatoLayout(Agraph_t *);
Agraph_t *twopiLayout(Agraph_t *);
Agraph_t *setDefaultAttrs(Agraph_t *);
#endif
