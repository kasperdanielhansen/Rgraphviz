#include "common.h"
#include "util.h"

SEXP R_scalarReal(double v) {
    SEXP ans = allocVector(REALSXP,1);
    REAL(ans)[0] = v;
    return(ans);
}

SEXP R_scalarInteger(int v)
{
  SEXP  ans = allocVector(INTSXP, 1);
  INTEGER(ans)[0] = v;
  return(ans);
}

SEXP R_scalarLogical(Rboolean v)
{
  SEXP  ans = allocVector(LGLSXP, 1);
  LOGICAL(ans)[0] = v;
  return(ans);
}

SEXP R_scalarString(const char *v)
{
  SEXP ans = allocVector(STRSXP, 1);
  PROTECT(ans);
  if(v)
    SET_STRING_ELT(ans, 0, mkChar(v));
  UNPROTECT(1);
  return(ans);
}

SEXP getListElement(SEXP list, char *str) {
    /* Given a R list and a character string, will return the */
    /* element of the list which has the name that corresponds to the */
    /*   string */
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;

    if (names == R_NilValue)
	error("Attribute vectors must have names");

    for (i = 0; i < length(list); i++) {
	if (strcmp(CHAR(STRING_ELT(names,i)), str) == 0) {
            if (TYPEOF(list) == VECSXP)
                elmt = VECTOR_ELT(list, i);
            else
                error("expecting VECSXP, got %s", 
                      Rf_type2char(TYPEOF(list))); 
	    break;
	}
    }
    return(elmt);
}

SEXP stringEltByName(SEXP strv, char *str) {
    /* Given STRSXP (character vector in R) and a string, return the
     * element of the strv (CHARSXP) which has the name that
     * corresponds to the string.
     */
    SEXP elmt = R_NilValue;
    SEXP names = GET_NAMES(strv);
    int i;

    if (names == R_NilValue)
	error("the character vector must have names");

    /* simple linear search */
    for (i = 0; i < length(strv); i++) {
	if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
            elmt = STRING_ELT(strv, i);
	    break;
	}
    }
    return(elmt);
}

int getVectorPos(SEXP vector, char *str) {
    /* Returns position in a named vector where the name matches string*/
    /* Returns -1 if not found */
    
    SEXP names; 
    int i;

    PROTECT(names = getAttrib(vector, R_NamesSymbol));
    for (i = 0; i < length(vector); i++) {
	if (strcmp(CHAR(STRING_ELT(names,i)),str) == 0)
	    break;
    }
    
    UNPROTECT(1);

    if (i == length(vector))
	i = -1;

    return(i);
}

SEXP Rgraphviz_fin(SEXP s) { 
    /* Finalizer for the external reference */
    Agraph_t *g;

    CHECK_Rgraphviz_graph(s);
    g = R_ExternalPtrAddr(s);
    agclose(g); 
    R_ClearExternalPtr(s);
    return(R_NilValue);
}

SEXP assignAttrs(SEXP attrList, SEXP objList,
			   SEXP defAttrs) {
    /* Assign attributes defined by attrList (and defAttrs) */
    /* to slots of the objects listed in objList            */
    int i, j, k, namePos, leno;
    SEXP curAttrs, curObj, attrNames, objNames;
    char* curObjName;
    SEXP attrsSlot, newASlot, oattrs;
    SEXP names, onames;
    SEXP attrPos;
    SEXP curSTR;

    PROTECT(attrNames = getAttrib(attrList, R_NamesSymbol));
    PROTECT(objNames = getAttrib(objList, R_NamesSymbol));
    PROTECT(defAttrs = coerceVector(defAttrs, STRSXP));
    for (i = 0; i < length(objList); i++) {
	curObj = VECTOR_ELT(objList, i);
	PROTECT(attrsSlot = GET_SLOT(curObj, Rf_install("attrs")));
        curObjName = CHAR(STRING_ELT(objNames, i));
	for (j = 0; j < length(attrList); j++) {
	    PROTECT(curSTR = allocVector(STRSXP, 1));
	    PROTECT(curAttrs = coerceVector(VECTOR_ELT(attrList, j), STRSXP));
	    PROTECT(attrPos = stringEltByName(curAttrs, curObjName));
	    if (attrPos == R_NilValue) {
		/* We need to use the default value here */
		UNPROTECT(1);
		attrPos = stringEltByName(defAttrs,
                                          CHAR(STRING_ELT(attrNames, j)));
                PROTECT(attrPos);
		       
		if (attrPos == R_NilValue) {
		    error("No attribute or default was assigned for %s",
			  STR(GET_SLOT(curObj, Rf_install("name"))));
		}
	    }
	    /* Now we have attrVal and need to add this to the node */
	    namePos = getVectorPos(attrsSlot,
				   CHAR(STRING_ELT(attrNames, j)));
	    if (namePos < 0) {
		/* This is a new element, need to expand the vector */		
		PROTECT(oattrs = attrsSlot);
		leno = length(oattrs);
		PROTECT(onames = getAttrib(attrsSlot, R_NamesSymbol));
		PROTECT(names = allocVector(STRSXP, leno+1));
		PROTECT(newASlot = allocVector(VECSXP, leno+1));
		for (k = 0; k < leno; k++) {
		    SET_VECTOR_ELT(newASlot, k, VECTOR_ELT(oattrs, k));
		    SET_STRING_ELT(names, k, STRING_ELT(onames, k));
		}

		/* Assign the new element */
		SET_STRING_ELT(curSTR, 0, attrPos);
		SET_VECTOR_ELT(newASlot, leno, curSTR);
		SET_STRING_ELT(names, leno, STRING_ELT(attrNames, j));
		setAttrib(newASlot, R_NamesSymbol, names);
		attrsSlot = newASlot;
		UNPROTECT(4);
	    }
	    else {
		    SET_STRING_ELT(curSTR, 0, attrPos);
		    SET_VECTOR_ELT(attrsSlot, namePos, curSTR);
	    }
	    UNPROTECT(3);
	}
	SET_SLOT(curObj, Rf_install("attrs"), attrsSlot);
	SET_VECTOR_ELT(objList, i, curObj);
	UNPROTECT(1);
    }

    UNPROTECT(3);

    return(objList);
}

SEXP buildRagraph(Agraph_t *g) {
    SEXP graphRef, klass, obj;

    PROTECT(graphRef = R_MakeExternalPtr(g,Rgraphviz_graph_type_tag,
				 R_NilValue));
    R_RegisterCFinalizer(graphRef, (R_CFinalizer_t)Rgraphviz_fin);

    klass = PROTECT(MAKE_CLASS("Ragraph"));
    PROTECT(obj = NEW_OBJECT(klass));
    
    SET_SLOT(obj, Rf_install("agraph"), graphRef);
    SET_SLOT(obj, Rf_install("laidout"), R_scalarLogical(FALSE));
    SET_SLOT(obj, Rf_install("numEdges"), R_scalarInteger(agnedges(g)));

    UNPROTECT(3);

    return(obj);
}

