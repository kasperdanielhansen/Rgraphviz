#include "common.h"
#include "util.h"

SEXP Rgraphviz_agread(SEXP filename) {
    Agraph_t *g;
    FILE *dotFile;

    dotFile = fopen(STR(filename),"r");
    if (dotFile == NULL) {
	error("Requested file does not exit");
    }
    aginit();
    g = agread(dotFile);

    return(buildRagraph(g));
}

