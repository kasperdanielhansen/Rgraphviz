#include "common.h"

static const R_CallMethodDef R_CallDef[] = {
    {"Rgraphviz_fin", (DL_FUNC)&Rgraphviz_fin, 1},
    {"Rgraphviz_agread", (DL_FUNC)&Rgraphviz_agread, 1},
    {"Rgraphviz_agwrite", (DL_FUNC)&Rgraphviz_agwrite, 2},
    {"Rgraphviz_agopen", (DL_FUNC)&Rgraphviz_agopen, 6},
    {"Rgraphviz_doLayout", (DL_FUNC)&Rgraphviz_doLayout, 2},
    {"buildRagraph", (DL_FUNC)&buildRagraph, 1},
    {"getBoundBox", (DL_FUNC)&getBoundBox, 1},
    {"getNodeLayouts", (DL_FUNC)&getNodeLayouts, 1},
    {"getEdgeLocs", (DL_FUNC)&getEdgeLocs, 2},
    {"Rgraphviz_graphvizVersion", (DL_FUNC)&Rgraphviz_graphvizVersion, 0},
    {"setDefaultAttrs", (DL_FUNC)&setDefaultAttrs, 1},
    {NULL, NULL, 0},
};

void R_init_Rgraphviz(DllInfo *info)
{
  R_registerRoutines(info,NULL,R_CallDef,NULL,NULL);
}
