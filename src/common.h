#ifndef RGRAPHVIZ_COMMON_H
#define RGRAPHVIZ_COMMON_H 1

#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <R_ext/RConverters.h>
#include <R_ext/Rdynload.h>

#ifndef WIN32
#include <unistd.h>
#endif

#include <math.h>

#if GRAPHVIZ_MAJOR == 2 

#if GRAPHVIZ_MINOR <= 3
#include <render.h>
#include <graph.h>
#include <dotprocs.h>
#include <neatoprocs.h>
#include <adjust.h>
#include <renderprocs.h>
#include <circle.h>
extern char *Info[];

#else

#include <gvc.h>
#include <gvplugin.h>

#if GRAPHVIZ_MINOR > 4
#include <gvcext.h>
#endif

#if GRAPHVIZ_MINOR >= 10
#include <gvcjob.h>
#endif

#include <gvcint.h>
#include <globals.h>
#endif

#endif

/* from libgraph.h, due to overlap with graph.h, cannot #include it */
extern Agdict_t *agdictof(void *);

/* these are missing in .h */
#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR <= 4
extern void circo_layout(Agraph_t * g);
extern void fdp_layout(Agraph_t * g);
#endif

extern GVC_t *gvc;
extern SEXP Rgraphviz_graph_type_tag;

typedef struct Rgattr_t Rgattr_t;
struct Rgattr_t { char* name; char* value; };

/* these are called by R-functions directly */
SEXP Rgraphviz_agopen(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP Rgraphviz_agread(SEXP);
SEXP Rgraphviz_agwrite(SEXP, SEXP);
SEXP Rgraphviz_bezier(SEXP, SEXP, SEXP);
SEXP Rgraphviz_buildNodeList(SEXP, SEXP, SEXP, SEXP);
SEXP Rgraphviz_buildEdgeList(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP Rgraphviz_doLayout(SEXP, SEXP);
SEXP Rgraphviz_graphvizVersion(void);
SEXP Rgraphviz_init(void);

SEXP Rgraphviz_toFile(SEXP, SEXP, SEXP, SEXP);

SEXP Rgraphviz_getDefAttrsGraph(SEXP);
SEXP Rgraphviz_setDefAttrsGraph(SEXP, SEXP, SEXP);
SEXP Rgraphviz_getAttrsGraph(SEXP, SEXP);
SEXP Rgraphviz_setAttrsGraph(SEXP, SEXP, SEXP, SEXP);

SEXP Rgraphviz_getDefAttrsCluster(SEXP, SEXP);
SEXP Rgraphviz_setDefAttrsCluster(SEXP, SEXP, SEXP, SEXP);
SEXP Rgraphviz_getAttrsCluster(SEXP, SEXP, SEXP);
SEXP Rgraphviz_setAttrsCluster(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP Rgraphviz_getDefAttrsNode(SEXP);
SEXP Rgraphviz_setDefAttrsNode(SEXP, SEXP, SEXP);
SEXP Rgraphviz_getAttrsNode(SEXP, SEXP, SEXP);
SEXP Rgraphviz_setAttrsNode(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP Rgraphviz_getDefAttrsEdge(SEXP);
SEXP Rgraphviz_setDefAttrsEdge(SEXP, SEXP, SEXP);
SEXP Rgraphviz_getAttrsEdge(SEXP, SEXP, SEXP, SEXP);
SEXP Rgraphviz_setAttrsEdge(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

#endif // RGRAPHVIZ_COMMON_H

