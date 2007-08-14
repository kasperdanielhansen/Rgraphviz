#include "common.h"
#include "util.h"

/*
 * dotneato.c in the graphviz sources defines char* Info[], but does
 * not make it extern.  We declare it extern in
 * Rgraphviz/src/common.h, but this doesn't seem to work on Windows.
 * So for now, we hard code the version of graphviz that we
 * hand-built.
 */
#ifdef Win32
SEXP Rgraphviz_graphvizVersion(void) {
    return(mkString("2.2.1"));
}
#else
SEXP Rgraphviz_graphvizVersion(void) {

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR <= 3
    return(Rgraphviz_ScalarStringOrNull(Info[1]));
#endif

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >=4 && GRAPHVIZ_MINOR <= 9
    return(Rgraphviz_ScalarStringOrNull(gvc->info[1]));
#endif

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >= 10 
    return(Rgraphviz_ScalarStringOrNull(gvc->common.info[1]));
#endif

}
#endif

