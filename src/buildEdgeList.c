#include "common.h"
#include "util.h"

SEXP Rgraphviz_buildEdgeList(SEXP edgeL, SEXP edgeMode, SEXP subGList,
                             SEXP edgeNames, SEXP removedEdges,
                             SEXP edgeAttrs, SEXP defAttrs) {
    int x, y, curEle = 0;
    SEXP from;
    SEXP peList;
    SEXP peClass, curPE;
    SEXP curAttrs, curFrom, curTo, curWeights = R_NilValue;
    SEXP attrNames;
    SEXP tmpToSTR, tmpWtSTR, tmpW;
    SEXP curSubG, subGEdgeL, subGEdges, elt;
    SEXP recipAttrs, newRecipAttrs, recipAttrNames, newRecipAttrNames;
    SEXP goodEdgeNames;
    SEXP toName;
    SEXP recipPE;
    char *edgeName, *recipName;
    int i, j, k, nSubG;
    int nEdges = length(edgeNames);

    if (length(edgeL) == 0)
        return(allocVector(VECSXP, 0));

    PROTECT(peClass = MAKE_CLASS("pEdge"));

    PROTECT(peList = allocVector(VECSXP, nEdges -
                                 length(removedEdges)));
    PROTECT(goodEdgeNames = allocVector(STRSXP, nEdges -
                                        length(removedEdges)));
    PROTECT(curAttrs = allocVector(VECSXP, 3));

    PROTECT(attrNames = allocVector(STRSXP, 3));

    /* TODO: get rid of attrs "arrowhead"/"arrowtail", "dir" is sufficient */
    SET_STRING_ELT(attrNames, 0, mkChar("arrowhead"));
    SET_STRING_ELT(attrNames, 1, mkChar("weight"));
    SET_STRING_ELT(attrNames, 2, mkChar("dir"));
    setAttrib(curAttrs, R_NamesSymbol, attrNames);

    PROTECT(from = getAttrib(edgeL, R_NamesSymbol));
    nSubG = length(subGList);

    /* For each edge, create a new object of class pEdge */
    /* and then assign the 'from' and 'to' strings as */
    /* as well as the default attrs (arrowhead & weight) */

    for (x = 0; x < length(from); x++) {
        PROTECT(curFrom = allocVector(STRSXP, 1));
        SET_STRING_ELT(curFrom, 0, STRING_ELT(from, x));
        if (length(VECTOR_ELT(edgeL, x)) == 0)
            error("Invalid edgeList element given to buildEdgeList in Rgraphviz, is NULL");
        PROTECT(curTo = coerceVector(VECTOR_ELT(VECTOR_ELT(edgeL, x), 0),
                                     INTSXP));
        if (length(VECTOR_ELT(edgeL, x)) > 1) {
            curWeights = VECTOR_ELT(VECTOR_ELT(edgeL, x), 1);
        }
        if (curWeights == R_NilValue || (length(curWeights) != length(curTo))) {
            curWeights = allocVector(REALSXP, length(curTo));
            for (i = 0; i < length(curWeights); i++)
                REAL(curWeights)[i] = 1;
        }
        PROTECT(curWeights);

        for (y = 0; y < length(curTo); y++) {
            PROTECT(toName = STRING_ELT(from, INTEGER(curTo)[y]-1));
            edgeName = (char *)malloc((strlen(STR(curFrom))+
                                       strlen(CHAR(toName)) + 2) *
                                      sizeof(char));
            snprintf(edgeName, sizeof(edgeName), "%s~%s", STR(curFrom), CHAR(toName));

            /* See if this edge is a removed edge */
            for (i = 0; i < length(removedEdges); i++) {
                if (strcmp(CHAR(STRING_ELT(edgeNames,
                                           INTEGER(removedEdges)[i]-1)),
                           edgeName) == 0)
                    break;
            }

            if (i < length(removedEdges)) {
                /* This edge is to be removed */
                if (strcmp(STR(edgeMode), "directed") == 0) {
                    /* Find the recip and add 'open' to tail */

                    recipName = (char *)malloc((strlen(STR(curFrom))+
                                                strlen(CHAR(toName)) + 2) *
                                               sizeof(char));
                    snprintf(recipName, sizeof(recipName), "%s~%s", CHAR(toName), STR(curFrom));

                    for (k = 0; k < curEle; k++) {
                        if (strcmp(CHAR(STRING_ELT(goodEdgeNames, k)),
                                   recipName) == 0)
                            break;
                    }
                    free(recipName);

                    PROTECT(recipPE = VECTOR_ELT(peList, k));

                    recipAttrs = GET_SLOT(recipPE, Rf_install("attrs"));
                    recipAttrNames = getAttrib(recipAttrs,
                                               R_NamesSymbol);
                    /* We need to add this to the current set of
                       recipAttrs, so create a new list which is one
                       element longer and copy everything over, adding
                       the new element */
                    PROTECT(newRecipAttrs = allocVector(VECSXP,
                                                        length(recipAttrs)+1));
                    PROTECT(newRecipAttrNames = allocVector(STRSXP,
                                                            length(recipAttrNames)+1));
                    for (j = 0; j < length(recipAttrs); j++) {
                      if ( !strcmp(CHAR(STRING_ELT(recipAttrNames, j)), "dir") )
                        SET_VECTOR_ELT(newRecipAttrs, j, mkString("both"));
                      else
                        SET_VECTOR_ELT(newRecipAttrs, j, 
                                       VECTOR_ELT(recipAttrs, j));

                      SET_STRING_ELT(newRecipAttrNames, j,
                                       STRING_ELT(recipAttrNames, j));
                    }

                    SET_VECTOR_ELT(newRecipAttrs, j, mkString("open"));
                    SET_STRING_ELT(newRecipAttrNames, j, mkChar("arrowtail"));
                    setAttrib(newRecipAttrs, R_NamesSymbol, newRecipAttrNames);

                    SET_SLOT(recipPE, Rf_install("attrs"), newRecipAttrs);
                    SET_VECTOR_ELT(peList, k, recipPE);
                    UNPROTECT(3);

                }
                UNPROTECT(1);
                continue;
            }
            PROTECT(tmpToSTR = allocVector(STRSXP, 1));
            PROTECT(curPE = NEW_OBJECT(peClass));
            SET_SLOT(curPE, Rf_install("from"), curFrom);
            SET_STRING_ELT(tmpToSTR, 0, toName);
            SET_SLOT(curPE, Rf_install("to"), tmpToSTR);
            if (strcmp(STR(edgeMode), "directed") == 0)
	    {
                SET_VECTOR_ELT(curAttrs, 0, mkString("open"));
                SET_VECTOR_ELT(curAttrs, 2, mkString("forward"));
            }
            else
	    {
                SET_VECTOR_ELT(curAttrs, 0, mkString("none"));
                SET_VECTOR_ELT(curAttrs, 2, mkString("none"));
            }
            PROTECT(tmpWtSTR = allocVector(STRSXP, 1));
            PROTECT(tmpW = Rf_ScalarReal(REAL(curWeights)[y]));
            SET_STRING_ELT(tmpWtSTR, 0, asChar(tmpW));
            UNPROTECT(1);
            SET_VECTOR_ELT(curAttrs, 1, tmpWtSTR);
            SET_SLOT(curPE, Rf_install("attrs"), curAttrs);
            SET_STRING_ELT(goodEdgeNames, curEle, mkChar(edgeName));
            SET_VECTOR_ELT(peList, curEle, curPE);
            curEle++;
            for (i = 0; i < nSubG; i++) {
                curSubG = getListElement(VECTOR_ELT(subGList, i), "graph");
                subGEdgeL = GET_SLOT(curSubG, Rf_install("edgeL"));
                elt = getListElement(subGEdgeL, STR(curFrom));
                if (elt == R_NilValue)
                    continue;
                /* Extract out the edges */
                subGEdges = VECTOR_ELT(elt, 0);
                for (j = 0; j < length(subGEdges); j++) {
                    if (INTEGER(subGEdges)[j] == INTEGER(curTo)[y])
                        break;
                }
                if (j == length(subGEdges))
                    continue;
                /* If we get here, then this edge is in subG 'i' */
                SET_SLOT(curPE, Rf_install("subG"), Rf_ScalarInteger(i+1));

                /* Only one subgraph per edge */
                break;
            }
            free(edgeName);
            UNPROTECT(4);
        }
        UNPROTECT(3);
    }
    setAttrib(peList, R_NamesSymbol, goodEdgeNames);
    peList = assignAttrs(edgeAttrs, peList, defAttrs);

    UNPROTECT(6);

    return(peList);
}

