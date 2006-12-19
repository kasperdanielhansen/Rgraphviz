#include "common.h"
#include "util.h"

SEXP Rgraphviz_graph_type_tag;
GVC_t *gvc = NULL;

SEXP Rgraphviz_init(void) {
    Rgraphviz_graph_type_tag = install("RGRAPH_TYPE_TAG");

    /* Stifle graphviz warning messages, only return errors */
    agseterr(AGERR);

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >= 4
    if ( gvc ) { gvFreeContext(gvc); gvc = NULL; }
    gvc = gvContext();
#endif 
    return(R_NilValue);
}

