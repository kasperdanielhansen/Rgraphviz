#ifndef __COMMON_H_INCLUDED
#define __COMMON_H_INCLUDED

#define DOTLAYOUT 0
#define NEATOLAYOUT 1
#define TWOPILAYOUT 2

#define MINGRAPHVIZVER "1.9.20030423.0415"

#define ENABLE_CODEGENS 1

#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/RConverters.h>
#include <R_ext/Rdynload.h>
#ifndef WIN32
#include <unistd.h>
#endif

#include <math.h>
#include <render.h>
#include <graph.h>
#include <dotprocs.h>
#include <neatoprocs.h>
#include <adjust.h>

#include <gvrender.h>

extern char *Info[];

#ifdef GRAPHVIZ_1_12
static GVC_t *gvc;
#endif

#define AGRAPH_T(x) ((agraph_t *)DATAPTR(x))
#define STR(SE) CHAR(STRING_ELT(SE,0))

SEXP R_scalarReal(double);
SEXP R_scalarInteger(int);
SEXP R_scalarLogical(Rboolean);
SEXP R_scalarString(const char *);
SEXP getListElement(SEXP, char*);
int getVectorPos(SEXP, char*);

SEXP Rgraphviz_init(void);
SEXP Rgraphviz_fin(SEXP);
SEXP Rgraphviz_doLayout(SEXP, SEXP);
SEXP getBoundBox(Agraph_t *);
SEXP getEdgeLocs(Agraph_t *,int);
SEXP Rgraphviz_agread(SEXP);
SEXP Rgraphviz_agwrite(SEXP, SEXP);
SEXP Rgraphviz_agopen(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP getNodeLayouts(Agraph_t *);
SEXP buildRagraph(Agraph_t *);
SEXP Rgraphviz_graphvizVersion(void);
SEXP Rgraphviz_getAttr(SEXP, SEXP);
Agraph_t *dotLayout(Agraph_t *);
Agraph_t *neatoLayout(Agraph_t *);
Agraph_t *twopiLayout(Agraph_t *);
Agraph_t *setDefaultAttrs(Agraph_t *, SEXP);
#endif
