#include "common.h"
#include "util.h"

#ifdef GRAPHVIZ_STATIC
extern gvplugin_library_t gvplugin_dot_layout_LTX_library;
extern gvplugin_library_t gvplugin_neato_layout_LTX_library;
extern gvplugin_library_t gvplugin_core_LTX_library;

lt_symlist_t lt_preloaded_symbols[] =
{
    { "gvplugin_dot_layout_LTX_library", (void*)(&gvplugin_dot_layout_LTX_library)},
    { "gvplugin_neato_layout_LTX_library", (void*)(&gvplugin_neato_layout_LTX_library)},
    { "gvplugin_core_LTX_library", (void*)(&gvplugin_core_LTX_library)},
    { 0, 0}
};
#endif

SEXP Rgraphviz_graph_type_tag;
GVC_t *gvc = NULL;

SEXP Rgraphviz_init(void) {
    Rgraphviz_graph_type_tag = install("RGRAPH_TYPE_TAG");

    /* Stifle graphviz warning messages, only return errors */
    agseterr(AGERR);

    if ( gvc ) { gvFreeContext(gvc); gvc = NULL; }
#ifdef GRAPHVIZ_STATIC
    gvc = gvContextPlugins(lt_preloaded_symbols,0);
#else
    gvc = gvContext();
#endif
    return(R_NilValue);
}
