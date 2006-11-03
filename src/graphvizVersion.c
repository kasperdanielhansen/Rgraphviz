#include "common.h"
#include "util.h"

/*
 * <FIXME>
 * dotneato.c in the graphviz sources defines char* Info[], but does
 * not make it extern.  We declare it extern in
 * Rgraphviz/src/common.h, but this doesn't seem to work on Windows.
 * So for now, we hard code the version of graphviz that we
 * hand-built.
 */
#ifdef Win32
SEXP Rgraphviz_graphvizVersion(void) {
    return(R_scalarString("2.2.1"));
}
#else
SEXP Rgraphviz_graphvizVersion(void) {
#ifndef GRAPHVIZGT_2_4
    return(R_scalarString(Info[1]));
#else
    return(R_scalarString(gvc->info[1]));
#endif
}
#endif
/* </FIXME> */

