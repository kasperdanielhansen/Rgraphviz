#include "common.h"
#include "util.h"

static Agraph_t *setDefaultAttrs(Agraph_t *g, SEXP attrs) {
    /* While attributes have default values already,  */
    /* if we want to dynamically set them, we need */
    /* to have defined defaults manually */
    int i;
    SEXP attrNames, elmt;
    char *cname, *celmt;

    /* Now set user defined attributes */
    /* Set the graph level attributes */
    PROTECT(elmt = getListElement(attrs, "graph"));
    /* Now elmt is a list of attributes to set */
    PROTECT(attrNames = getAttrib(elmt, R_NamesSymbol));
    for (i = 0; i < length(elmt); i++) {
        cname = ALLOC_CHAR(attrNames, i);
        celmt = ALLOC_CHAR(coerceVector(VECTOR_ELT(elmt,i), STRSXP), 0);
        agraphattr(g, cname, celmt);
        R_Free(celmt); R_Free(cname);
    }

    UNPROTECT(2);

    /* Now do node-wide */
    PROTECT(elmt = getListElement(attrs, "node"));
    PROTECT(attrNames = getAttrib(elmt, R_NamesSymbol));
    for (i = 0; i < length(elmt); i++) {
        cname = ALLOC_CHAR(attrNames,i);
        celmt = ALLOC_CHAR(coerceVector(VECTOR_ELT(elmt,i), STRSXP), 0);
        agnodeattr(g, cname, celmt);
        R_Free(celmt); R_Free(cname);
    }
    UNPROTECT(2);

    /* Lastly do edge-wide */
    PROTECT(elmt = getListElement(attrs, "edge"));
    PROTECT(attrNames = getAttrib(elmt, R_NamesSymbol));
    for (i = 0; i < length(elmt); i++) {
        cname = ALLOC_CHAR(attrNames, i);
        celmt = ALLOC_CHAR(coerceVector(VECTOR_ELT(elmt,i), STRSXP), 0);
        agedgeattr(g, cname, celmt);
        R_Free(celmt); R_Free(cname);
    }
    UNPROTECT(2);
    return(g);
}

SEXP Rgraphviz_agopen(SEXP name, SEXP kind, SEXP nodes,
                      SEXP edges, SEXP attrs, SEXP subGs) {
    /* Will create a new Agraph_t* object in graphviz and then */
    /* a Ragraph S4 object around it, returning it to R */
    Agraph_t *g, *tmpGraph;
    Agraph_t **sgs;
    Agnode_t *head, *tail, *tmp;
    Agedge_t *curEdge;
    char *subGName;
    char *cname, *celmt;
    int ag_k = 0;
    int i,j;
    int whichSubG;
    SEXP pNode, curPN, pEdge, curPE;
    SEXP attrNames, curAttrs, curSubG, curSubGEle;

    PROTECT(pNode = MAKE_CLASS("pNode"));
    PROTECT(pEdge = MAKE_CLASS("pEdge"));

    if (!isInteger(kind))
        error("kind must be an integer value");
    else
        ag_k = INTEGER(kind)[0];

    if ((ag_k < 0)||(ag_k > 3))
        error("kind must be an integer value between 0 and 3");

    if (!isString(name))
        error("name must be a string");

    aginit();
    cname = ALLOC_CHAR(name, 0);
    g = agopen(cname, ag_k);
    R_Free(cname);

    /* Set default attributes */
    g = setDefaultAttrs(g,attrs);

    /* Allocate space in the subgraph array */
    sgs = (Agraph_t **)R_alloc(length(subGs), sizeof(Agraph_t *));
    if ((length(subGs) > 0) && (sgs == NULL))
        error("Out of memory while allocating subgraphs");

    if (length(subGs) > 0) {
        /* Create any subgraphs, if necessary */
        for (i = 0; i < length(subGs); i++) {
            curSubG = VECTOR_ELT(subGs, i);

            /* First see if this is a cluster or not */
            curSubGEle = getListElement(curSubG, "cluster");
            subGName = (char *)malloc(100 * sizeof(char));
            if ((curSubGEle == R_NilValue)||
                    (LOGICAL(curSubGEle)[0] == TRUE))
                snprintf(subGName, sizeof(subGName), "%s%d", "cluster_", i);
            else
                snprintf(subGName, sizeof(subGName), "%d", i);

            sgs[i] = agsubg(g, subGName);

            free(subGName);

            /* Now assign attrs */
            curSubGEle = getListElement(curSubG, "attrs");
            if (curSubGEle != R_NilValue) {
                attrNames = getAttrib(curSubGEle, R_NamesSymbol);
                for (j = 0; j < length(curSubGEle); j++) {
                    cname = ALLOC_CHAR(attrNames, j);
                    celmt = ALLOC_CHAR(curSubGEle, j);
                    agset(sgs[i], cname, celmt);
                    R_Free(celmt); R_Free(cname);
                }
            }
        }
    }

    /* Get the nodes created */
    for (i = 0; i < length(nodes); i++) {
        PROTECT(curPN = VECTOR_ELT(nodes, i));

        /* Need to check the node # against the subG vector */
        /* And assign it to the proper graph, not necessarily 'g' */
        whichSubG = INTEGER(GET_SLOT(curPN, Rf_install("subG")))[0];
        if (whichSubG > 0) {
            /* Point tmpGraph to the appropriate current graph */
            /* Remember that in R they're numbered 1->X and in */
            /* C it is 0-(X-1) */
            tmpGraph = sgs[whichSubG-1];
        }
        else
            tmpGraph = g;

        cname = ALLOC_CHAR(GET_SLOT(curPN, Rf_install("name")), 0);
        tmp = agnode(tmpGraph, cname);
        R_Free(cname);

        PROTECT(curAttrs = coerceVector(GET_SLOT(curPN, Rf_install("attrs")), STRSXP));
        PROTECT(attrNames = coerceVector(getAttrib(curAttrs, R_NamesSymbol), STRSXP));
        for (j = 0; j < length(curAttrs); j++) {
            cname = ALLOC_CHAR(attrNames, j);
            celmt = ALLOC_CHAR(curAttrs, j);
            agset(tmp,  cname, celmt);
            R_Free(cname); R_Free(celmt);
        }

        UNPROTECT(3);
    }

    /* now fill in the edges */
    for (i = 0; i < length(edges); i++) {
        PROTECT(curPE = VECTOR_ELT(edges, i));

        whichSubG = INTEGER(GET_SLOT(curPE, Rf_install("subG")))[0];
        if (whichSubG > 0) {
            tmpGraph = sgs[whichSubG-1];
        }
        else {
            tmpGraph = g;
        }

        cname = ALLOC_CHAR(GET_SLOT(curPE, Rf_install("from")), 0);
        tail = agfindnode(g, cname);
        R_Free(cname);
        if (tail == NULL)
            error("Missing tail node");

        cname = ALLOC_CHAR(GET_SLOT(curPE, Rf_install("to")), 0);
        head = agfindnode(g, cname);
        R_Free(cname);
        if (head == NULL) error("Missing head node");

        curEdge = agedge(tmpGraph, tail, head); 

        PROTECT(curAttrs = GET_SLOT(curPE, Rf_install("attrs")));
        PROTECT(attrNames = getAttrib(curAttrs, R_NamesSymbol));
        for (j = 0; j < length(curAttrs); j++) {
            cname = ALLOC_CHAR(attrNames,j);
            celmt = ALLOC_CHAR(VECTOR_ELT(curAttrs, j), 0);
            agset(curEdge, cname, celmt);
            R_Free(celmt); R_Free(cname);
        }
        UNPROTECT(3);
    }

    UNPROTECT(2);
    return(buildRagraph(g));
}

