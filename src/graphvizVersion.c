#include "common.h"
#include "util.h"

SEXP Rgraphviz_graphvizVersion(void) {
    return(Rgraphviz_ScalarStringOrNull(gvcVersion(gvc)));
}

SEXP Rgraphviz_capabilities(void) {
    SEXP capabilities, names, capabilities_layout, capabilities_render, 
	capabilities_textlayout, capabilities_device, capabilities_loadimage;
    
    PROTECT(capabilities = NEW_LIST(5));
    PROTECT(names = NEW_CHARACTER(5));

    int count, cc;
    char **lp;

    /* layoutTypes */
    lp = gvPluginList(gvc, "layout", &count, NULL);
    PROTECT(capabilities_layout = NEW_CHARACTER(count));
    for (cc = 0; cc < count; cc++) {
	SET_STRING_ELT(capabilities_layout, cc, mkChar(lp[cc]));
    }
    SET_VECTOR_ELT(capabilities, 0, capabilities_layout);
    SET_STRING_ELT(names, 0, mkChar("layoutTypes"));
    UNPROTECT(1);
    
    lp = gvPluginList(gvc, "render", &count, NULL);
    PROTECT(capabilities_render = NEW_CHARACTER(count));
    for (cc = 0; cc < count; cc++) {
	SET_STRING_ELT(capabilities_render, cc, mkChar(lp[cc]));
    }
    SET_VECTOR_ELT(capabilities, 1, capabilities_render);
    SET_STRING_ELT(names, 1, mkChar("renderTypes"));
    UNPROTECT(1);

    lp = gvPluginList(gvc, "textlayout", &count, NULL);
    PROTECT(capabilities_textlayout = NEW_CHARACTER(count));
    for (cc = 0; cc < count; cc++) {
	SET_STRING_ELT(capabilities_textlayout, cc, mkChar(lp[cc]));
    }
    SET_VECTOR_ELT(capabilities, 2, capabilities_textlayout);
    SET_STRING_ELT(names, 2, mkChar("textlayoutTypes"));
    UNPROTECT(1);
    
    lp = gvPluginList(gvc, "device", &count, NULL);
    PROTECT(capabilities_device = NEW_CHARACTER(count));
    for (cc = 0; cc < count; cc++) {
	SET_STRING_ELT(capabilities_device, cc, mkChar(lp[cc]));
    }
    SET_VECTOR_ELT(capabilities, 3, capabilities_device);
    SET_STRING_ELT(names, 3, mkChar("deviceTypes"));
    UNPROTECT(1);

    lp = gvPluginList(gvc, "loadimage", &count, NULL);
    PROTECT(capabilities_loadimage = NEW_CHARACTER(count));
    for (cc = 0; cc < count; cc++) {
	SET_STRING_ELT(capabilities_loadimage, cc, mkChar(lp[cc]));
    }
    SET_VECTOR_ELT(capabilities, 4, capabilities_loadimage);
    SET_STRING_ELT(names, 4, mkChar("loadimageTypes"));
    UNPROTECT(1);

    setAttrib(capabilities, R_NamesSymbol, names);
    UNPROTECT(2);
    return(capabilities);
}

