#include "common.h"
#include "util.h"

SEXP Rgraphviz_buildNodeList(SEXP nodes, SEXP nodeAttrs,
			     SEXP subGList, SEXP defAttrs) {
    SEXP pNodes;
    SEXP pnClass, curPN;
    SEXP attrs, attrNames, tmpStr;
    SEXP curSubG, subGNodes;
    int i, j, k, nSubG;

    nSubG = length(subGList);

    PROTECT(pnClass = MAKE_CLASS("pNode"));

    PROTECT(pNodes = allocVector(VECSXP, length(nodes)));

    PROTECT(attrNames = allocVector(STRSXP, 1));
    SET_STRING_ELT(attrNames, 0, mkChar("label"));

    for (i = 0; i < length(nodes); i++) {
	PROTECT(tmpStr = allocVector(STRSXP, 1));
	SET_STRING_ELT(tmpStr, 0, STRING_ELT(nodes, i));
	PROTECT(curPN = NEW_OBJECT(pnClass));
	SET_SLOT(curPN, Rf_install("name"), tmpStr);

	PROTECT(attrs = allocVector(VECSXP, 1));
	setAttrib(attrs, R_NamesSymbol, attrNames);

	SET_VECTOR_ELT(attrs, 0, tmpStr);
	SET_SLOT(curPN, Rf_install("attrs"), attrs);
	SET_VECTOR_ELT(pNodes, i, curPN);
	
	for (j = 0; j < nSubG; j++) {
	    curSubG = getListElement(VECTOR_ELT(subGList, j), "graph");
	    subGNodes = GET_SLOT(curSubG, Rf_install("nodes"));

	    for (k = 0; k < length(subGNodes); k++) {
		if (strcmp(CHAR(STRING_ELT(subGNodes, k)),
			   CHAR(STRING_ELT(nodes, i))) == 0)
		    break;
	    }
	    if (k == length(subGNodes))
		continue;

	    SET_SLOT(curPN, Rf_install("subG"), R_scalarInteger(j+1));
	    /* Only one subgraph per node */
	    break;
	}

	UNPROTECT(3);
    }

    setAttrib(pNodes, R_NamesSymbol, nodes);

    /* Put any attributes associated with this node list in */
    pNodes = assignAttrs(nodeAttrs, pNodes, defAttrs);

    UNPROTECT(3);
    return(pNodes);
}

