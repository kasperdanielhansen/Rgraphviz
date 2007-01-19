#include "common.h"
#include "util.h"

SEXP Rgraphviz_agwrite(SEXP graph, SEXP filename) {
    /* Takes an R graph and writes it out in DOT format to a file */
    Agraph_t *g;
    FILE *dotFile;
    SEXP slotTmp;

    /* extract the Agraph_t* external reference from the R object */
    slotTmp = GET_SLOT(graph, Rf_install("agraph"));
    CHECK_Rgraphviz_graph(slotTmp);
    g = R_ExternalPtrAddr(slotTmp);

    /* output the Agraph_t */
    dotFile = fopen(STR(filename),"w");
    if (dotFile == NULL) {
        error("Error opening file");
    }
    agwrite(g, dotFile);

    fclose(dotFile);
    return(R_NilValue);
}

