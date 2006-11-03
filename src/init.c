#include "common.h"
#include "util.h"

SEXP Rgraphviz_graph_type_tag;
GVC_t *gvc;

SEXP Rgraphviz_init(void) {
    Rgraphviz_graph_type_tag = install("RGRAPH_TYPE_TAG");

    /* Stifle graphviz warning messages, only return errors */
    agseterr(AGERR);

#ifdef GRAPHVIZGT_2_4
    gvc = gvContext();
#endif /* GRAPHVIZGT_2_4 */

    return(R_NilValue);
}

