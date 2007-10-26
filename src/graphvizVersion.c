#include "common.h"
#include "util.h"

/*
 * dotneato.c in the graphviz sources defines char* Info[], but does
 * not make it extern.  We declare it extern in
 * Rgraphviz/src/common.h, but this doesn't seem to work on Windows.
 * So for now, we hard code the version of graphviz that we
 * hand-built.
 */
SEXP Rgraphviz_graphvizVersion(void) {

#ifdef Win32

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >= 15
    return(Rgraphviz_ScalarStringOrNull(gvcVersion(gvc)));
#else
    return(mkString("Unsupported graphviz version on Windows"));
#endif

#elif GRAPHVIZ_MAJOR == 2 

#if GRAPHVIZ_MINOR <= 3
    return(Rgraphviz_ScalarStringOrNull(Info[1]));

#elif GRAPHVIZ_MINOR >=4 && GRAPHVIZ_MINOR <= 9
    return(Rgraphviz_ScalarStringOrNull(gvc->info[1]));

#elif GRAPHVIZ_MINOR >= 10  && GRAPHVIZ_MINOR < 14
    return(Rgraphviz_ScalarStringOrNull(gvc->common.info[1]));

#else /* GRAPHVIZ_MINOR >= 14 */
    return(Rgraphviz_ScalarStringOrNull(gvcVersion(gvc)));

#endif

#else
    return(mkString("Unknown graphviz version"));
#endif

}

