#include "common.h"
#include "util.h"

SEXP Rgraphviz_graph_type_tag;
GVC_t *gvc = NULL;

SEXP Rgraphviz_init(void) {
    Rgraphviz_graph_type_tag = install("RGRAPH_TYPE_TAG");

    /* Stifle graphviz warning messages, only return errors */
    agseterr(AGERR);

#ifdef GRAPHVIZGT_2_4
    if ( gvc ) { gvFreeContext(gvc); gvc = NULL; }
    gvc = gvContext();
#endif /* GRAPHVIZGT_2_4 */

    return(R_NilValue);
}

