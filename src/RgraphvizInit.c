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
            /*
                {"Rgraphviz_toFile", (DL_FUNC)&Rgraphviz_toFile, 4),
            */
            {NULL, NULL, 0},
        };

void R_init_Rgraphviz(DllInfo *info)
{
    R_registerRoutines(info,NULL,R_CallDef,NULL,NULL);
}
