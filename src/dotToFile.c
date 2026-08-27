#include "common.h"
#include "util.h"
#include <gvc.h>

Agraph_t* myg = NULL;

SEXP Rgraphviz_dotToFile(SEXP graph, SEXP layoutType, SEXP filename, SEXP fileType)
{
  char * char_graph = ALLOC_CHAR(graph, 0);
  myg = agmemread(char_graph);
  gvLayout(gvc, myg, ALLOC_CHAR(layoutType, 0));
  gvRenderFilename(gvc, myg, ALLOC_CHAR(fileType, 0), ALLOC_CHAR(filename, 0) );
  gvFreeLayout(gvc, myg);
  agclose(myg);
  return(R_NilValue);
   
}

