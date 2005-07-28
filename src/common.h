#ifndef __COMMON_H_INCLUDED
#define __COMMON_H_INCLUDED

#define DOTLAYOUT 0
#define NEATOLAYOUT 1
#define TWOPILAYOUT 2
#define CIRCOLAYOUT 3
#define FDPLAYOUT 4

#define MINGRAPHVIZVER "1.9.20030423.0415"

#define ENABLE_CODEGENS 1

#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <R_ext/RConverters.h>
#include <R_ext/Rdynload.h>
#ifndef WIN32
#include <unistd.h>
#endif

#include <math.h>

#ifdef GRAPHVIZGT_2_4
#include <gvc.h>
#else
#include <render.h>
#include <graph.h>
#include <dotprocs.h>
#include <neatoprocs.h>
#include <adjust.h>
#include <renderprocs.h>
#endif

/*#include <gvrender.h> */

extern char *Info[];

#ifndef GRAPHVIZ_1_16
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
SEXP assignAttrs(SEXP, SEXP, SEXP);
SEXP Rgraphviz_bezier(SEXP, SEXP, SEXP);
SEXP Rgraphviz_buildNodeList(SEXP, SEXP, SEXP, SEXP);
SEXP Rgraphviz_buildEdgeList(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP generatePNodes(SEXP, SEXP);

#ifndef GRAPHVIZGT_1_16
Agraph_t *dotLayout(Agraph_t *);
Agraph_t *neatoLayout(Agraph_t *);
Agraph_t *twopiLayout(Agraph_t *);
#endif

Agraph_t *setDefaultAttrs(Agraph_t *, SEXP);
#endif
