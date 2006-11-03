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
			// TODO: mod this Rf_ call (rev. 18855 -> 19051)
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

SEXP Rgraphviz_getAttr(SEXP graph, SEXP attr) {
    Agraph_t *g;
    SEXP slotTmp;

    PROTECT(slotTmp = GET_SLOT(graph, install("agraph")));
    CHECK_Rgraphviz_graph(slotTmp);
    g = R_ExternalPtrAddr(slotTmp);
    UNPROTECT(1);

    return(R_scalarString(agget(g, STR(attr))));
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


SEXP getBoundBox(Agraph_t *g) {
    /* Determine the graphviz determiend bounding box and */
    /* assign it to the appropriate Ragraph structure */
    SEXP bbClass, xyClass, curBB, LLXY, URXY;

    xyClass = MAKE_CLASS("xyPoint");
    bbClass = MAKE_CLASS("boundingBox");

    PROTECT(curBB = NEW_OBJECT(bbClass));
    PROTECT(LLXY = NEW_OBJECT(xyClass));
    PROTECT(URXY = NEW_OBJECT(xyClass));

    SET_SLOT(LLXY,Rf_install("x"),R_scalarInteger(g->u.bb.LL.x));
    SET_SLOT(LLXY,Rf_install("y"),R_scalarInteger(g->u.bb.LL.y));
    SET_SLOT(URXY,Rf_install("x"),R_scalarInteger(g->u.bb.UR.x));
    SET_SLOT(URXY,Rf_install("y"),R_scalarInteger(g->u.bb.UR.y));

    SET_SLOT(curBB,Rf_install("botLeft"), LLXY);
    SET_SLOT(curBB,Rf_install("upRight"), URXY);

    UNPROTECT(3);
    return(curBB);
}

SEXP getNodeLayouts(Agraph_t *g) {
    Agnode_t *node;
    SEXP outLst, nlClass, xyClass, curXY, curNL;
    SEXP curLab, labClass;
    int i, nodes;
    char *tmpString;

    if (g == NULL)
	error("getNodeLayouts passed a NULL graph");

    nlClass = MAKE_CLASS("AgNode");
    xyClass = MAKE_CLASS("xyPoint");
    labClass = MAKE_CLASS("AgTextLabel");

    /* tmpString is used to convert a char to a char* w/ labels */
    tmpString = (char *)R_alloc(2, sizeof(char));
    if (tmpString == NULL)
	error("Allocation error in getNodeLayouts");

    nodes = agnnodes(g);
    node = agfstnode(g);

    PROTECT(outLst = allocVector(VECSXP, nodes));

    for (i = 0; i < nodes; i++) {	
	PROTECT(curNL = NEW_OBJECT(nlClass));
	PROTECT(curXY = NEW_OBJECT(xyClass));
	SET_SLOT(curXY,Rf_install("x"),R_scalarInteger(node->u.coord.x));
	SET_SLOT(curXY,Rf_install("y"),R_scalarInteger(node->u.coord.y));
	SET_SLOT(curNL,Rf_install("center"),curXY);
	SET_SLOT(curNL,Rf_install("height"),R_scalarInteger(node->u.ht));
	SET_SLOT(curNL,Rf_install("rWidth"),R_scalarInteger(node->u.rw));
	SET_SLOT(curNL,Rf_install("lWidth"),R_scalarInteger(node->u.lw));
	SET_SLOT(curNL,Rf_install("name"), R_scalarString(node->name));

	SET_SLOT(curNL, Rf_install("color"), 
		 R_scalarString(agget(node, "color")));
	SET_SLOT(curNL, Rf_install("fillcolor"),
		 R_scalarString(agget(node, "fillcolor")));
	SET_SLOT(curNL, Rf_install("shape"),
		 R_scalarString(agget(node, "shape")));
	SET_SLOT(curNL, Rf_install("style"),
		 R_scalarString(agget(node, "style")));


	PROTECT(curLab = NEW_OBJECT(labClass));
	if (node->u.label->u.txt.line != NULL) {
	    SET_SLOT(curLab, Rf_install("labelText"),
		     R_scalarString(node->u.label->u.txt.line->str));
	    snprintf(tmpString, 2, "%c",node->u.label->u.txt.line->just);
	    SET_SLOT(curLab, Rf_install("labelJust"),
		     R_scalarString(tmpString));
	    
	    SET_SLOT(curLab, Rf_install("labelWidth"),
		     R_scalarInteger(node->u.label->u.txt.line->width));
	    
	    /* Get the X/Y location of the label */
	    PROTECT(curXY = NEW_OBJECT(xyClass));
	    SET_SLOT(curXY, Rf_install("x"),
		     R_scalarInteger(node->u.label->p.x));
	    SET_SLOT(curXY, Rf_install("y"),
		     R_scalarInteger(node->u.label->p.y));
	    SET_SLOT(curLab, Rf_install("labelLoc"), curXY);
	    UNPROTECT(1);
	    
	    SET_SLOT(curLab, Rf_install("labelColor"),
		     R_scalarString(node->u.label->fontcolor));
	    
	    SET_SLOT(curLab, Rf_install("labelFontsize"),
 		     R_scalarReal(node->u.label->fontsize));
    
	}

	SET_SLOT(curNL, Rf_install("txtLabel"), curLab);
	
	SET_ELEMENT(outLst, i, curNL);
	node = agnxtnode(g,node);
	    
	UNPROTECT(3);
    }
    UNPROTECT(1);
    return(outLst);
}

SEXP getEdgeLocs(Agraph_t *g, int numEdges) {
    SEXP outList, curCP, curEP, pntList, pntSet, curXY, curLab;
    SEXP epClass, cpClass, xyClass, labClass;
    Agnode_t *node, *head;
    Agedge_t *edge;
    char *tmpString;
    bezier bez;
    int nodes;
    int i,k,l,pntLstEl;
    int curEle = 0;

    epClass = MAKE_CLASS("AgEdge");
    cpClass = MAKE_CLASS("BezierCurve");
    xyClass = MAKE_CLASS("xyPoint");
    labClass = MAKE_CLASS("AgTextLabel");

    /* tmpString is used to convert a char to a char* w/ labels */
    tmpString = (char *)R_alloc(2, sizeof(char));
    if (tmpString == NULL)
	error("Allocation error in getEdgeLocs");

    PROTECT(outList = allocVector(VECSXP, numEdges));

    nodes = agnnodes(g);
    node = agfstnode(g);

    for (i = 0; i < nodes; i++) {
	edge = agfstout(g, node);
	while (edge != NULL) {
	    PROTECT(curEP = NEW_OBJECT(epClass));
	    bez = edge->u.spl->list[0];
	    PROTECT(pntList = allocVector(VECSXP, 
					  ((bez.size-1)/3)));
	    pntLstEl = 0;

	    /* There are really (bez.size-1)/3 sets of control */
	    /* points, with the first set containing teh first 4 */
	    /* points, and then every other set starting with the */
	    /* last point from the previous set and then the next 3 */
	    for (k = 1; k < bez.size; k += 3) {
		PROTECT(curCP = NEW_OBJECT(cpClass));
		PROTECT(pntSet = allocVector(VECSXP, 4));
		for (l = -1; l < 3; l++) {
		    PROTECT(curXY = NEW_OBJECT(xyClass));
		    SET_SLOT(curXY, Rf_install("x"), 
			     R_scalarInteger(bez.list[k+l].x));
		    SET_SLOT(curXY, Rf_install("y"), 
			     R_scalarInteger(bez.list[k+l].y));
		    SET_ELEMENT(pntSet, l+1, curXY);
		    UNPROTECT(1);
		}
		SET_SLOT(curCP, Rf_install("cPoints"), pntSet);
		SET_ELEMENT(pntList, pntLstEl++, curCP);
		UNPROTECT(2);
	    }	    
	    SET_SLOT(curEP, Rf_install("splines"), pntList);
	    /* get the sp and ep */
	    PROTECT(curXY = NEW_OBJECT(xyClass));
	    SET_SLOT(curXY, Rf_install("x"),
		     R_scalarInteger(bez.sp.x));
	    SET_SLOT(curXY, Rf_install("y"),
		     R_scalarInteger(bez.sp.y));
	    SET_SLOT(curEP, Rf_install("sp"), curXY);
	    UNPROTECT(1);
	    PROTECT(curXY = NEW_OBJECT(xyClass));
	    SET_SLOT(curXY, Rf_install("x"),
		     R_scalarInteger(bez.ep.x));
	    SET_SLOT(curXY, Rf_install("y"),
		     R_scalarInteger(bez.ep.y));
	    SET_SLOT(curEP, Rf_install("ep"), curXY);
	    UNPROTECT(1);	    

	    SET_SLOT(curEP, Rf_install("tail"), 
		     R_scalarString(node->name));
	    head = edge->head;
	    SET_SLOT(curEP, Rf_install("head"),
		     R_scalarString(head->name));

	    SET_SLOT(curEP, Rf_install("arrowhead"),
		     R_scalarString(agget(edge, "arrowhead")));
	    SET_SLOT(curEP, Rf_install("arrowtail"),
		     R_scalarString(agget(edge, "arrowtail")));
	    SET_SLOT(curEP, Rf_install("arrowsize"),
		     R_scalarString(agget(edge, "arrowsize")));

	    SET_SLOT(curEP, Rf_install("color"), 
		     R_scalarString(agget(edge, "color")));

	    /* Get the label information */
	    if (edge->u.label != NULL) {
		PROTECT(curLab = NEW_OBJECT(labClass));
		SET_SLOT(curLab, Rf_install("labelText"),
			 R_scalarString(edge->u.label->u.txt.line->str));
		/* Get the X/Y location of the label */
		PROTECT(curXY = NEW_OBJECT(xyClass));
		SET_SLOT(curXY, Rf_install("x"),
			 R_scalarInteger(edge->u.label->p.x));
		SET_SLOT(curXY, Rf_install("y"),
			 R_scalarInteger(edge->u.label->p.y));
		SET_SLOT(curLab, Rf_install("labelLoc"), curXY);
		UNPROTECT(1);
			 
		snprintf(tmpString, 2, "%c",edge->u.label->u.txt.line->just);
		SET_SLOT(curLab, Rf_install("labelJust"),
			 R_scalarString(tmpString));

		SET_SLOT(curLab, Rf_install("labelWidth"),
			 R_scalarInteger(edge->u.label->u.txt.line->width));

		SET_SLOT(curLab, Rf_install("labelColor"),
			 R_scalarString(edge->u.label->fontcolor));

		SET_SLOT(curLab, Rf_install("labelFontsize"),
			 R_scalarReal(edge->u.label->fontsize));

		SET_SLOT(curEP, Rf_install("txtLabel"), curLab);
		UNPROTECT(1);
	    }

	    SET_ELEMENT(outList, curEle++, curEP);
	    UNPROTECT(2);
	    edge = agnxtout(g, edge);
	}
	node = agnxtnode(g, node);
    }
    UNPROTECT(1);

    return(outList);
}

Agraph_t *setDefaultAttrs(Agraph_t *g, SEXP attrs) {
    /* While attributes have default values already,  */
    /* if we want to dynamically set them, we need */
    /* to have defined defaults manually */
    int i;
    SEXP attrNames, elmt;

    /* Now set user defined attributes */
    /* Set the graph level attributes */
    PROTECT(elmt = getListElement(attrs, "graph"));
    /* Now elmt is a list of attributes to set */
    PROTECT(attrNames = getAttrib(elmt, R_NamesSymbol));
    for (i = 0; i < length(elmt); i++) {
	agraphattr(g, CHAR(STRING_ELT(attrNames,i)),
		   STR(coerceVector(VECTOR_ELT(elmt,i), STRSXP)));
    }
    
    UNPROTECT(2);

    /* Now do node-wide */
    PROTECT(elmt = getListElement(attrs, "node"));
    PROTECT(attrNames = getAttrib(elmt, R_NamesSymbol));
    for (i = 0; i < length(elmt); i++) {
	agnodeattr(g, CHAR(STRING_ELT(attrNames,i)),
		   STR(coerceVector(VECTOR_ELT(elmt,i), STRSXP)));
    }
    UNPROTECT(2);

    /* Lastly do edge-wide */
    PROTECT(elmt = getListElement(attrs, "edge"));
    PROTECT(attrNames = getAttrib(elmt, R_NamesSymbol));
    for (i = 0; i < length(elmt); i++) {
	agedgeattr(g, CHAR(STRING_ELT(attrNames,i)),
		   STR(coerceVector(VECTOR_ELT(elmt,i), STRSXP)));
   }
    UNPROTECT(2);
    return(g);
}

