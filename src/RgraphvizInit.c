#include "common.h"

static const R_CallMethodDef R_CallDef[] = {
             {"Rgraphviz_agread", (DL_FUNC)&Rgraphviz_agread, 1},
             {"Rgraphviz_agwrite", (DL_FUNC)&Rgraphviz_agwrite, 2},
             {"Rgraphviz_agopen", (DL_FUNC)&Rgraphviz_agopen, 6},
             {"Rgraphviz_doLayout", (DL_FUNC)&Rgraphviz_doLayout, 2},

             {"Rgraphviz_graphvizVersion", (DL_FUNC)&Rgraphviz_graphvizVersion, 0},
             {"Rgraphviz_bezier", (DL_FUNC)&Rgraphviz_bezier, 3},
             {"Rgraphviz_buildNodeList", (DL_FUNC)&Rgraphviz_buildNodeList, 4},
             {"Rgraphviz_buildEdgeList", (DL_FUNC)&Rgraphviz_buildEdgeList, 7},

             {"Rgraphviz_toFile", (DL_FUNC)&Rgraphviz_toFile, 4},
             {"Rgraphviz_dotToFile", (DL_FUNC)&Rgraphviz_dotToFile, 4},

             {"Rgraphviz_getDefAttrsGraph", (DL_FUNC)&Rgraphviz_getDefAttrsGraph, 1},
             {"Rgraphviz_setDefAttrsGraph", (DL_FUNC)&Rgraphviz_setDefAttrsGraph, 3},
             {"Rgraphviz_getAttrsGraph", (DL_FUNC)&Rgraphviz_getAttrsGraph, 2},
             {"Rgraphviz_setAttrsGraph", (DL_FUNC)&Rgraphviz_setAttrsGraph, 4},
         
             {"Rgraphviz_getAttrsCluster", (DL_FUNC)&Rgraphviz_getAttrsCluster, 3},
             {"Rgraphviz_setAttrsCluster", (DL_FUNC)&Rgraphviz_setAttrsCluster, 5},
   
             {"Rgraphviz_getDefAttrsNode", (DL_FUNC)&Rgraphviz_getDefAttrsNode, 1},
             {"Rgraphviz_setDefAttrsNode", (DL_FUNC)&Rgraphviz_setDefAttrsNode, 3},
             {"Rgraphviz_getAttrsNode", (DL_FUNC)&Rgraphviz_getAttrsNode, 3},
             {"Rgraphviz_setAttrsNode", (DL_FUNC)&Rgraphviz_setAttrsNode, 5},
         
             {"Rgraphviz_getDefAttrsEdge", (DL_FUNC)&Rgraphviz_getDefAttrsEdge, 1},
             {"Rgraphviz_setDefAttrsEdge", (DL_FUNC)&Rgraphviz_setDefAttrsEdge, 3},
             {"Rgraphviz_getAttrsEdge", (DL_FUNC)&Rgraphviz_getAttrsEdge, 4},
             {"Rgraphviz_setAttrsEdge", (DL_FUNC)&Rgraphviz_setAttrsEdge, 6},

             {NULL, NULL, 0},
             };

void R_init_Rgraphviz(DllInfo *info)
{
    R_registerRoutines(info,NULL,R_CallDef,NULL,NULL);
}
