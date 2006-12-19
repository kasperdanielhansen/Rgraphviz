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

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR <= 3
#include <render.h>
#include <graph.h>
#include <dotprocs.h>
#include <neatoprocs.h>
#include <adjust.h>
#include <renderprocs.h>
#include <circle.h>
extern char *Info[];
#endif

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >= 4 && GRAPHVIZ_MINOR <= 9
#include <gvc.h>
#include <gvplugin.h>
#include <gvcext.h>
#include <gvcint.h>
#include <globals.h>
#endif

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >= 10
#include <gvc.h>
#include <gvplugin.h>
#include <gvcext.h>
#include <gvcjob.h>
#include <gvcint.h>
#include <globals.h>
#endif

extern GVC_t *gvc;
extern SEXP Rgraphviz_graph_type_tag;

typedef struct Rgattr_t Rgattr_t;
struct Rgattr_t { char* name; char* value; };

// these are called by R-functions directly
SEXP Rgraphviz_agopen(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP Rgraphviz_agread(SEXP);
SEXP Rgraphviz_agwrite(SEXP, SEXP);
SEXP Rgraphviz_bezier(SEXP, SEXP, SEXP);
SEXP Rgraphviz_buildNodeList(SEXP, SEXP, SEXP, SEXP);
SEXP Rgraphviz_buildEdgeList(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP Rgraphviz_doLayout(SEXP, SEXP);
SEXP Rgraphviz_graphvizVersion(void);
SEXP Rgraphviz_init(void);

#endif // RGRAPHVIZ_COMMON_H

