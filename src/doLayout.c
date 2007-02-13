#include "common.h"
#include "util.h"

SEXP getBoundBox(Agraph_t *g) {
    /* Determine the graphviz determiend bounding box and */
    /* assign it to the appropriate Ragraph structure */
    SEXP bbClass, xyClass, curBB, LLXY, URXY;

    xyClass = MAKE_CLASS("xyPoint");
    bbClass = MAKE_CLASS("boundingBox");

    PROTECT(curBB = NEW_OBJECT(bbClass));
    PROTECT(LLXY = NEW_OBJECT(xyClass));
    PROTECT(URXY = NEW_OBJECT(xyClass));

    SET_SLOT(LLXY,Rf_install("x"),Rf_ScalarInteger(g->u.bb.LL.x));
    SET_SLOT(LLXY,Rf_install("y"),Rf_ScalarInteger(g->u.bb.LL.y));
    SET_SLOT(URXY,Rf_install("x"),Rf_ScalarInteger(g->u.bb.UR.x));
    SET_SLOT(URXY,Rf_install("y"),Rf_ScalarInteger(g->u.bb.UR.y));

    SET_SLOT(curBB,Rf_install("botLeft"), LLXY);
    SET_SLOT(curBB,Rf_install("upRight"), URXY);

    UNPROTECT(3);
    return(curBB);
}

SEXP getEdgeLocs(Agraph_t *g) {
    SEXP outList, curCP, curEP, pntList, pntSet, curXY, curLab;
    SEXP epClass, cpClass, xyClass, labClass;
    Agnode_t *node, *head;
    Agedge_t *edge;
    char *tmpString;
    char *tmp_ed;
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
    if (tmpString == NULL) error("Allocation error in getEdgeLocs");

    PROTECT(outList = allocVector(VECSXP, agnedges(g)));

    nodes = agnnodes(g);
    node = agfstnode(g);

    for (i = 0; i < nodes; i++) {
        edge = agfstout(g, node);
        while (edge != NULL) {
            PROTECT(curEP = NEW_OBJECT(epClass));
            bez = edge->u.spl->list[0];
            PROTECT(pntList = allocVector(VECSXP, ((bez.size-1)/3)));
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
                    SET_SLOT(curXY, Rf_install("x"), Rf_ScalarInteger(bez.list[k+l].x));
                    SET_SLOT(curXY, Rf_install("y"), Rf_ScalarInteger(bez.list[k+l].y));
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
            SET_SLOT(curXY, Rf_install("x"), Rf_ScalarInteger(bez.sp.x));
            SET_SLOT(curXY, Rf_install("y"), Rf_ScalarInteger(bez.sp.y));
            SET_SLOT(curEP, Rf_install("sp"), curXY);
            UNPROTECT(1);
            PROTECT(curXY = NEW_OBJECT(xyClass));
            SET_SLOT(curXY, Rf_install("x"), Rf_ScalarInteger(bez.ep.x));
            SET_SLOT(curXY, Rf_install("y"), Rf_ScalarInteger(bez.ep.y));
            SET_SLOT(curEP, Rf_install("ep"), curXY);
            UNPROTECT(1);

            SET_SLOT(curEP, Rf_install("tail"), Rgraphviz_ScalarStringOrNull(node->name));
            head = edge->head;
            SET_SLOT(curEP, Rf_install("head"), Rgraphviz_ScalarStringOrNull(head->name));

            /* TODO: clean up the use of attrs: dir, arrowhead, arrowtail.
             * the following are for interactive plotting in R-env, not needed 
             * for output to files.  The existing codes set "dir"-attr, but use
             * "arrowhead"/"arrowtail" instead.  Quite confusing.  
             */
            tmp_ed = agget(edge, "dir");
            if ( !AG_IS_DIRECTED(g) || !tmp_ed )	/* undirected graph */
            {
                SET_SLOT(curEP, Rf_install("arrowhead"), Rgraphviz_ScalarStringOrNull(agget(edge, "arrowhead")));
                SET_SLOT(curEP, Rf_install("arrowtail"), Rgraphviz_ScalarStringOrNull(agget(edge, "arrowtail")));
                SET_SLOT(curEP, Rf_install("arrowsize"), Rgraphviz_ScalarStringOrNull(agget(edge, "arrowsize")));
            }
            else if ( strcmp(tmp_ed, "both")==0 )
            {
                SET_SLOT(curEP, Rf_install("arrowhead"), Rgraphviz_ScalarStringOrNull("open"));
                SET_SLOT(curEP, Rf_install("arrowtail"), Rgraphviz_ScalarStringOrNull("open"));
                SET_SLOT(curEP, Rf_install("arrowsize"), Rgraphviz_ScalarStringOrNull("1"));
            }
            else /* if ( strcmp(tmp_ed, "forward")==0 ) */
            {
                SET_SLOT(curEP, Rf_install("arrowhead"), Rgraphviz_ScalarStringOrNull("open"));
                SET_SLOT(curEP, Rf_install("arrowtail"), Rgraphviz_ScalarStringOrNull("none"));
                SET_SLOT(curEP, Rf_install("arrowsize"), Rgraphviz_ScalarStringOrNull("1"));
            }

            SET_SLOT(curEP, Rf_install("color"), Rgraphviz_ScalarStringOrNull(agget(edge, "color")));

            /* get lty/lwd info */
            if ( agget(edge, "lty") )
               SET_SLOT(curEP, Rf_install("lty"), Rgraphviz_ScalarStringOrNull(agget(edge, "lty")));

            if ( agget(edge, "lwd") )
               SET_SLOT(curEP, Rf_install("lwd"), Rgraphviz_ScalarStringOrNull(agget(edge, "lwd")));

            /* Get the label information */
            if (edge->u.label != NULL) {
                PROTECT(curLab = NEW_OBJECT(labClass));
#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >= 10
                SET_SLOT(curLab, Rf_install("labelText"),
                         Rgraphviz_ScalarStringOrNull(ED_label(edge)->u.txt.para->str));
#else
                SET_SLOT(curLab, Rf_install("labelText"),
                         Rgraphviz_ScalarStringOrNull(edge->u.label->u.txt.line->str));
#endif
                /* Get the X/Y location of the label */
                PROTECT(curXY = NEW_OBJECT(xyClass));
                SET_SLOT(curXY, Rf_install("x"), Rf_ScalarInteger(edge->u.label->p.x));
                SET_SLOT(curXY, Rf_install("y"), Rf_ScalarInteger(edge->u.label->p.y));
                SET_SLOT(curLab, Rf_install("labelLoc"), curXY);
                UNPROTECT(1);


#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >= 10
                snprintf(tmpString, 2, "%c",ED_label(edge)->u.txt.para->just);
                SET_SLOT(curLab, Rf_install("labelJust"),
                         Rgraphviz_ScalarStringOrNull(tmpString));
#else
                snprintf(tmpString, 2, "%c",edge->u.label->u.txt.line->just);
                SET_SLOT(curLab, Rf_install("labelJust"), Rgraphviz_ScalarStringOrNull(tmpString));
#endif

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >= 10
                SET_SLOT(curLab, Rf_install("labelWidth"),
                         Rf_ScalarInteger(ED_label(edge)->u.txt.para->width));
#else
                SET_SLOT(curLab, Rf_install("labelWidth"),
                         Rf_ScalarInteger(edge->u.label->u.txt.line->width));
#endif

                SET_SLOT(curLab, Rf_install("labelColor"),
                         Rgraphviz_ScalarStringOrNull(edge->u.label->fontcolor));

                SET_SLOT(curLab, Rf_install("labelFontsize"), Rf_ScalarReal(edge->u.label->fontsize));

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

SEXP getNodeLayouts(Agraph_t *g) {
    Agnode_t *node;
    SEXP outLst, nlClass, xyClass, curXY, curNL;
    SEXP curLab, labClass;
    int i, nodes;
    char *tmpString;

    if (g == NULL) error("getNodeLayouts passed a NULL graph");

    nlClass = MAKE_CLASS("AgNode");
    xyClass = MAKE_CLASS("xyPoint");
    labClass = MAKE_CLASS("AgTextLabel");

    /* tmpString is used to convert a char to a char* w/ labels */
    tmpString = (char *)R_alloc(2, sizeof(char));
    if (tmpString == NULL) error("Allocation error in getNodeLayouts");

    nodes = agnnodes(g);
    node = agfstnode(g);

    PROTECT(outLst = allocVector(VECSXP, nodes));

    for (i = 0; i < nodes; i++) {
        PROTECT(curNL = NEW_OBJECT(nlClass));
        PROTECT(curXY = NEW_OBJECT(xyClass));
        SET_SLOT(curXY,Rf_install("x"),Rf_ScalarInteger(node->u.coord.x));
        SET_SLOT(curXY,Rf_install("y"),Rf_ScalarInteger(node->u.coord.y));
        SET_SLOT(curNL,Rf_install("center"),curXY);
        SET_SLOT(curNL,Rf_install("height"),Rf_ScalarInteger(node->u.ht));
        SET_SLOT(curNL,Rf_install("rWidth"),Rf_ScalarInteger(node->u.rw));
        SET_SLOT(curNL,Rf_install("lWidth"),Rf_ScalarInteger(node->u.lw));
        SET_SLOT(curNL,Rf_install("name"), Rgraphviz_ScalarStringOrNull(node->name));

        SET_SLOT(curNL, Rf_install("color"), Rgraphviz_ScalarStringOrNull(agget(node, "color")));
        SET_SLOT(curNL, Rf_install("fillcolor"), Rgraphviz_ScalarStringOrNull(agget(node, "fillcolor")));
        SET_SLOT(curNL, Rf_install("shape"), Rgraphviz_ScalarStringOrNull(agget(node, "shape")));
        SET_SLOT(curNL, Rf_install("style"), Rgraphviz_ScalarStringOrNull(agget(node, "style")));


        PROTECT(curLab = NEW_OBJECT(labClass));

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >= 10
        if (ND_label(node)->u.txt.para != NULL) {
            SET_SLOT(curLab, Rf_install("labelText"),
                     Rgraphviz_ScalarStringOrNull(ND_label(node)->u.txt.para->str));
            snprintf(tmpString, 2, "%c",ND_label(node)->u.txt.para->just);
            SET_SLOT(curLab, Rf_install("labelJust"), Rgraphviz_ScalarStringOrNull(tmpString));
#else
        if (node->u.label->u.txt.line != NULL) {
            SET_SLOT(curLab, Rf_install("labelText"), Rgraphviz_ScalarStringOrNull(node->u.label->u.txt.line->str));
            snprintf(tmpString, 2, "%c",node->u.label->u.txt.line->just);
            SET_SLOT(curLab, Rf_install("labelJust"), Rgraphviz_ScalarStringOrNull(tmpString));
#endif

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >= 10
            SET_SLOT(curLab, Rf_install("labelWidth"),
                     Rf_ScalarInteger(ND_label(node)->u.txt.para->width));
#else
            SET_SLOT(curLab, Rf_install("labelWidth"),
                     Rf_ScalarInteger(node->u.label->u.txt.line->width));
#endif

            /* Get the X/Y location of the label */
            PROTECT(curXY = NEW_OBJECT(xyClass));
            SET_SLOT(curXY, Rf_install("x"), Rf_ScalarInteger(node->u.label->p.x));
            SET_SLOT(curXY, Rf_install("y"), Rf_ScalarInteger(node->u.label->p.y));
            SET_SLOT(curLab, Rf_install("labelLoc"), curXY);
            UNPROTECT(1);

            SET_SLOT(curLab, Rf_install("labelColor"), Rgraphviz_ScalarStringOrNull(node->u.label->fontcolor));

            SET_SLOT(curLab, Rf_install("labelFontsize"), Rf_ScalarReal(node->u.label->fontsize));

        }

        SET_SLOT(curNL, Rf_install("txtLabel"), curLab);

        SET_ELEMENT(outLst, i, curNL);
        node = agnxtnode(g,node);

        UNPROTECT(3);
    }
    UNPROTECT(1);
    return(outLst);
}

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR <= 3
enum {
    DOTLAYOUT = 0,
    NEATOLAYOUT,
    TWOPILAYOUT,
    CIRCOLAYOUT,
    FDPLAYOUT
};
#else
static char *layouts[] = { "dot", "neato", "twopi", "circo", "fdp"};
#endif

SEXP Rgraphviz_doLayout(SEXP graph, SEXP layoutType) {
    /* Will perform a Graphviz layout on a graph */

    Agraph_t *g;
    SEXP slotTmp, nLayout, cPoints, bb;

    /* Extract the Agraph_t pointer from the S4 object */
    PROTECT(slotTmp = GET_SLOT(graph, install("agraph")));
    CHECK_Rgraphviz_graph(slotTmp);
    g = R_ExternalPtrAddr(slotTmp);

    /* Call the appropriate Graphviz layout routine */
    if (!isInteger(layoutType))
        error("layoutType must be an integer value");
    else {
        /* Note that we're using the standard dotneato */
        /* layout commands for layouts and not the ones */
        /* provided below.  This is a test */
#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR <= 3
        switch(INTEGER(layoutType)[0]) {
        case DOTLAYOUT:
            dot_layout(g);
            break;
        case NEATOLAYOUT:
            neato_layout(g);
            break;
        case TWOPILAYOUT:
            twopi_layout(g);
            break;
        case CIRCOLAYOUT:
            circo_layout(g);
            break;
#ifndef Win32
        case FDPLAYOUT:
            fdp_layout(g);
            break;
#endif
        default:
            error("Invalid layout type\n");
        }
#else
        gvLayout(gvc, g, layouts[INTEGER(layoutType)[0]]);
#endif
    }

    /* Here we want to extract information for the resultant S4
       object */
    PROTECT(nLayout = getNodeLayouts(g));
    PROTECT(bb = getBoundBox(g));
    PROTECT(cPoints = getEdgeLocs(g));
    SET_SLOT(graph, Rf_install("agraph"), slotTmp);
    SET_SLOT(graph,Rf_install("AgNode"), nLayout);
    SET_SLOT(graph,Rf_install("laidout"), Rgraphviz_ScalarLogicalFromRbool(TRUE));
    SET_SLOT(graph,Rf_install("AgEdge"), cPoints);
    SET_SLOT(graph,Rf_install("boundBox"), bb);
    UNPROTECT(4);

#if GRAPHVIZ_MAJOR == 2 && GRAPHVIZ_MINOR >= 4
    /* free gvc after rendering */
    gvFreeLayout(gvc, g);
#endif

    return(graph);
}

