#include "common.h"
#include "util.h"
#include <stdio.h>

static inline Agraph_t* getAgraphPtr(SEXP graph)
{
    SEXP slotTmp = GET_SLOT(graph, Rf_install("agraph"));
    CHECK_Rgraphviz_graph(slotTmp);
    Agraph_t *g = R_ExternalPtrAddr(slotTmp);
    return g;
}

#define CLUSTERFLAG "cluster"

static Agraph_t* getClusterPtr(SEXP graph, SEXP cluster)
{
    Agraph_t *g = getAgraphPtr(graph);
    if ( !g ) return(NULL);

    int i = INTEGER(cluster)[0];
    char subGName[256];
    snprintf(subGName, sizeof(subGName), "%s_%d", CLUSTERFLAG, i);

    Agraph_t *sg = agfindsubg(g, subGName);

    return(sg);
}

static void getDefAttrs(void *obj, int *n, char*** attr_name, char*** attr_defval)
{
    Agdict_t *dict = agdictof(obj);
    int i;

    if ( (*n = dtsize(dict->dict)) )
    {
       *attr_name = R_Calloc(*n, char*);
       *attr_defval = R_Calloc(*n, char*);

       for ( i = 0; i < *n; i++ )
       {
          (*attr_defval)[i] = dict->list[i]->value;
          (*attr_name)[i] = dict->list[i]->name;
       }
    }
}

/*
 * TODO:
 * o. validate ptrs to graph/node/edge
 * -- getAttrs
 * o. handle non-exist attr
 * o. handle given default attr
 * -- setAttrs
 * o. check attr default/val
 * o. ??? validate attr default/val, e.g., red for color, 123 not for shape ???
 * o. call "agclose(g)" somewhere...
*/

SEXP Rgraphviz_getDefAttrsGraph(SEXP graph)
{
    Agraph_t *g = getAgraphPtr(graph);
    if ( !g ) return(R_NilValue);

    int i = 0, nattr = 0; 
    char **attr_name = NULL, **attr_defval = NULL;

    getDefAttrs(g, &nattr, &attr_name, &attr_defval);

    SEXP ans;
    PROTECT(ans = allocMatrix(STRSXP, nattr, 2));

    for ( i = 0; i < nattr; i++ )
    {
        SET_STRING_ELT(ans, i, mkChar(attr_name[i]));
        SET_STRING_ELT(ans, nattr+i, mkChar(attr_defval[i]));
    }

    UNPROTECT(1);

    R_Free(attr_name); R_Free(attr_defval);

    return(ans);
}

SEXP Rgraphviz_setDefAttrsGraph(SEXP graph, SEXP attrname, SEXP attrval)
{
    char *cname, *cval;
    Agraph_t *g = getAgraphPtr(graph);
    if ( !g ) return(R_NilValue);

    cname = ALLOC_CHAR(attrname, 0);
    cval = ALLOC_CHAR(attrval, 0);
    Agsym_t *r = agraphattr(g, cname, cval);
    R_Free(cval); R_Free(cname);

    SEXP ans;
    PROTECT(ans = NEW_LOGICAL(1));
    LOGICAL(ans)[0] = r? TRUE : FALSE;
    UNPROTECT(1);
    return(ans);
}

SEXP Rgraphviz_getAttrsGraph(SEXP graph, SEXP attrname)
{
    Agraph_t *g = getAgraphPtr(graph);
    if ( !g ) return(R_NilValue);

    char *cname = ALLOC_CHAR(attrname, 0);
    char *val = agget(g, cname);
    R_Free(cname);

    if ( !val ) /* no such attr */
    {
       return(R_NilValue);
    }
    else 
    {
       SEXP ans;
       PROTECT(ans = allocVector(STRSXP, 1));
       SET_STRING_ELT(ans, 0, mkChar(val));
       UNPROTECT(1);
       return(ans);
    }
}

SEXP Rgraphviz_setAttrsGraph(SEXP graph,
                             SEXP attrname, SEXP attrval, SEXP default_val)
{
    Agraph_t *g = getAgraphPtr(graph);
    if ( !g ) return(R_NilValue);

    /* 0 for success, -1 otherwise */
    char *cname = ALLOC_CHAR(attrname, 0),
        *cval = ALLOC_CHAR(attrval, 0),
        *cdefault = ALLOC_CHAR(default_val, 0);
    int r = agsafeset(g, cname, cval, cdefault);
    R_Free(cdefault); R_Free(cval); R_Free(cname);

    SEXP ans;
    PROTECT(ans = NEW_LOGICAL(1));
    LOGICAL(ans)[0] = r? FALSE : TRUE;
    UNPROTECT(1);
    return(ans);
}

SEXP Rgraphviz_getAttrsCluster(SEXP graph, SEXP cluster, SEXP attrname)
{
    Agraph_t *sg = getClusterPtr(graph, cluster);
    if ( !sg ) return(R_NilValue);

    char *cname = ALLOC_CHAR(attrname, 0);
    char *val = agget(sg, cname);
    R_Free(cname);

    if ( !val ) /* no such attr */
    {
       return(R_NilValue);
    }
    else 
    {
       SEXP ans;
       PROTECT(ans = allocVector(STRSXP, 1));
       SET_STRING_ELT(ans, 0, mkChar(val));
       UNPROTECT(1);
       return(ans);
    }
}

SEXP Rgraphviz_setAttrsCluster(SEXP graph, SEXP cluster,
                               SEXP attrname, SEXP attrval, SEXP default_val)
{
    Agraph_t *sg = getClusterPtr(graph, cluster);
    if ( !sg ) return(R_NilValue);
    
    /* 0 for success, -1 otherwise */
    char *cname = ALLOC_CHAR(attrname, 0),
        *cval = ALLOC_CHAR(attrval, 0),
        *cdefault = ALLOC_CHAR(default_val, 0);
    int r = agsafeset(sg, cname, cval, cdefault);
    R_Free(cdefault); R_Free(cval); R_Free(cname);

    SEXP ans;
    PROTECT(ans = NEW_LOGICAL(1));
    LOGICAL(ans)[0] = r? FALSE : TRUE;
    UNPROTECT(1);
    return(ans);
}

SEXP Rgraphviz_getDefAttrsNode(SEXP graph)
{
    Agraph_t *g = getAgraphPtr(graph);
    if ( !g ) return(R_NilValue);

    Agnode_t *n = g->proto->n;

    int i = 0, nattr = 0;
    char **attr_name = NULL, **attr_defval = NULL;

    getDefAttrs(n, &nattr, &attr_name, &attr_defval);

    SEXP ans;
    PROTECT(ans = allocMatrix(STRSXP, nattr, 2));

    for ( i = 0; i < nattr; i++ )
    {
        SET_STRING_ELT(ans, i, mkChar(attr_name[i]));
        SET_STRING_ELT(ans, nattr+i, mkChar(attr_defval[i]));
    }

    UNPROTECT(1);

    R_Free(attr_name); R_Free(attr_defval);

    return(ans);
}

SEXP Rgraphviz_setDefAttrsNode(SEXP graph, SEXP attrname, SEXP attrval)
{
    Agraph_t *g = getAgraphPtr(graph);
    if ( !g ) return(R_NilValue);

    char *cname = ALLOC_CHAR(attrname, 0),
        *cval = ALLOC_CHAR(attrval, 0);
    Agsym_t *r = agnodeattr(g, cname, cval);
    R_Free(cval); R_Free(cname);

    SEXP ans;
    PROTECT(ans = NEW_LOGICAL(1));
    LOGICAL(ans)[0] = r? TRUE : FALSE;
    UNPROTECT(1);
    return(ans);
}

SEXP Rgraphviz_getAttrsNode(SEXP graph, SEXP node, SEXP attrname)
{
    Agraph_t *g = getAgraphPtr(graph);
    if ( !g ) return(R_NilValue);

    char *cnode = ALLOC_CHAR(node, 0);
    Agnode_t *n = agfindnode(g, cnode);
    R_Free(cnode);
    if ( !n ) return(R_NilValue);

    char *cval = ALLOC_CHAR(attrname, 0);
    char *val = agget(n, cval);
    R_Free(cval);

    if ( !val ) /* no such attr */
    {
        return(R_NilValue);
    }
    else 
    {
        SEXP ans;
        PROTECT(ans = allocVector(STRSXP, 1));
        SET_STRING_ELT(ans, 0, mkChar(val));
        UNPROTECT(1);
        return(ans);
    }
}

SEXP Rgraphviz_setAttrsNode(SEXP graph, SEXP node,
                            SEXP attrname, SEXP attrval, SEXP default_val)
{
    Agraph_t *g = getAgraphPtr(graph);
    if ( !g ) return(R_NilValue);

    char *cnode = ALLOC_CHAR(node, 0);
    Agnode_t *n = agfindnode(g, cnode);
    R_Free(cnode);
    if ( !n ) return(R_NilValue);

    char *cname = ALLOC_CHAR(attrname, 0),
        *cval = ALLOC_CHAR(attrval, 0),
        *cdefault = ALLOC_CHAR(default_val, 0);
    int r = agsafeset(n, cname, cval, cdefault);
    R_Free(cdefault); R_Free(cval); R_Free(cname);

    SEXP ans;
    PROTECT(ans = NEW_LOGICAL(1));
    LOGICAL(ans)[0] = r? FALSE : TRUE;
    UNPROTECT(1);
    return(ans);
}

SEXP Rgraphviz_getDefAttrsEdge(SEXP graph)
{
    Agraph_t *g = getAgraphPtr(graph);
    if ( !g ) return(R_NilValue);

    Agedge_t *e = g->proto->e;

    int i = 0, nattr = 0;
    char **attr_name = NULL, **attr_defval = NULL;

    getDefAttrs(e, &nattr, &attr_name, &attr_defval);

    SEXP ans;
    PROTECT(ans = allocMatrix(STRSXP, nattr, 2));

    for ( i = 0; i < nattr; i++ )
    {
        SET_STRING_ELT(ans, i, mkChar(attr_name[i]));
        SET_STRING_ELT(ans, nattr+i, mkChar(attr_defval[i]));
    }

    UNPROTECT(1);

    R_Free(attr_name); R_Free(attr_defval);

    return(ans);
}

SEXP Rgraphviz_setDefAttrsEdge(SEXP graph, SEXP attrname, SEXP attrval)
{
    Agraph_t *g = getAgraphPtr(graph);
    if ( !g ) return(R_NilValue);

    char *cname = ALLOC_CHAR(attrname, 0),
        *cval = ALLOC_CHAR(attrval, 0);
    Agsym_t *r = agedgeattr(g, cname, cval);
    R_Free(cval); R_Free(cname);

    SEXP ans;
    PROTECT(ans = NEW_LOGICAL(1));
    LOGICAL(ans)[0] = r? TRUE : FALSE;
    UNPROTECT(1);
    return(ans);
}

SEXP Rgraphviz_getAttrsEdge(SEXP graph, SEXP from, SEXP to, SEXP attrname)
{
    Agraph_t *g = getAgraphPtr(graph);
    char *cval;
    if ( !g ) return(R_NilValue);

    cval = ALLOC_CHAR(from, 0);
    Agnode_t *u = agfindnode(g, cval);
    R_Free(cval);
    cval = ALLOC_CHAR(to, 0);
    Agnode_t *v = agfindnode(g, cval);
    R_Free(cval);
    if ( !u || !v ) return(R_NilValue);

    Agedge_t *e = agfindedge(g, u, v);
    if ( !e ) return(R_NilValue);

    cval = ALLOC_CHAR(attrname, 0);
    char *val = agget(e, cval);
    R_Free(cval);

    if ( !val ) /* no such attr */
    {
       return(R_NilValue);
    }
    else 
    {
       SEXP ans;
       PROTECT(ans = allocVector(STRSXP, 1));
       SET_STRING_ELT(ans, 0, mkChar(val));
       UNPROTECT(1);
       return(ans);
    }
}

SEXP Rgraphviz_setAttrsEdge(SEXP graph, SEXP from, SEXP to,
                            SEXP attrname, SEXP attrval, SEXP default_val)
{
    Agraph_t *g = getAgraphPtr(graph);
    char *cval, *cname, *cdefault;
    if ( !g ) return(R_NilValue);

    cval = ALLOC_CHAR(from, 0);
    Agnode_t *u = agfindnode(g, cval);
    R_Free(cval);
    cval = ALLOC_CHAR(to, 0);
    Agnode_t *v = agfindnode(g, cval);
    R_Free(cval);
    if ( !u || !v ) return(R_NilValue);

    Agedge_t *e = agfindedge(g, u, v);
    if ( !e ) return(R_NilValue);

    cname = ALLOC_CHAR(attrname, 0);
    cval = ALLOC_CHAR(attrval, 0);
    cdefault = ALLOC_CHAR(default_val, 0);
    int r = agsafeset(e, cname, cval, cdefault);
    R_Free(cdefault); R_Free(cval); R_Free(cname); 

    SEXP ans;
    PROTECT(ans = NEW_LOGICAL(1));
    LOGICAL(ans)[0] = r? FALSE : TRUE;
    UNPROTECT(1);
    return(ans);
}

SEXP Rgraphviz_toFile(SEXP graph, SEXP layoutType, SEXP filename, SEXP filetype)
{
    Agraph_t *g = getAgraphPtr(graph);
    char *clayout, *cfiletype, *cfilename;
    if ( !g ) return(R_NilValue);

    clayout = ALLOC_CHAR(layoutType, 0);
    gvLayout(gvc, g, clayout);

    cfilename = ALLOC_CHAR(filename, 0);
    cfiletype = ALLOC_CHAR(filetype, 0);
    gvRenderFilename(gvc, g, cfiletype, cfilename);
    gvFreeLayout(gvc, g);
    R_Free(cfiletype); R_Free(cfilename);
    return(R_NilValue);
}

/*
 * g: graphNEL
 * nodes = nodes(g), 	strings
 * edges_from = edgeMatrix(g)["from",], edges_to = edgeMatrix(g)["to", ],  ints
 * nsubG = no. of subgraphs
 * subGIndex = subgraph-index for nodes, ints
 * recipK = combined reciprocal directed edges or not
*/
SEXP Rgraphviz_agopenSimple(SEXP name, SEXP kind,
              SEXP nodes, SEXP subGIndex,
              SEXP edges_from, SEXP edges_to,
              SEXP nsubG, SEXP subGs,
              SEXP recipK)
{
    Agraph_t *g, *tmpGraph;
    Agraph_t **sgs;
    Agnode_t *head, *tail, *curNode;
    Agedge_t *curEdge;
    SEXP curSubG, curSubGEle;

    int recip = INTEGER(recipK)[0];
    char subGName[256];
    char *cname, *cnode;
    int ag_k = 0;
    int nsg = INTEGER(nsubG)[0];
    int i;
    int whichSubG;

    if ( length(edges_from) != length(edges_to) )
        error("length of edges_from must be equal to length of edges_to");

    if ( length(nodes) != length(subGIndex) )
        error("length of nodes must be equal to length of subGIndex");

    if (!isString(name)) error("name must be a string");

    if (!isInteger(kind)) error("kind must be an integer value");

    ag_k = INTEGER(kind)[0];
    if ((ag_k < 0)||(ag_k > 3))
        error("kind must be an integer value between 0 and 3");

    aginit();
    cname = ALLOC_CHAR(name, 0);
    g = agopen(cname, ag_k);
    R_Free(cname);

    /* create subgraphs */
    sgs = (Agraph_t **)R_alloc(nsg, sizeof(Agraph_t *));
    if ( nsg > 0 && !sgs )
        error("Out of memory while allocating subgraphs");

    for (i = 0; i < nsg; i++) {
        curSubG = VECTOR_ELT(subGs, i);

        // First see if this is a cluster or not
        curSubGEle = getListElement(curSubG, CLUSTERFLAG);
        if ( curSubGEle == R_NilValue || LOGICAL(curSubGEle)[0] )
            snprintf(subGName, sizeof(subGName), "%s_%d", CLUSTERFLAG, i+1);
        else
            snprintf(subGName, sizeof(subGName), "%d", i+1);

        sgs[i] = agsubg(g, subGName);
    }

#if DEBUG
    printf(" nodes: ");
    for (i = 0; i < length(nodes); i++) {
        printf("%s ", CHAR(STRING_ELT(nodes, i)));
    }
    printf("\n");
    printf(" edges: ");
    for (i = 0; i < length(edges_from); i++) {
        printf("%d - %d     %s - %s \n",
               INTEGER(edges_from)[i],
               INTEGER(edges_to)[i],
               CHAR(STRING_ELT(nodes, INTEGER(edges_from)[i]-1)),
               CHAR(STRING_ELT(nodes, INTEGER(edges_to)[i]-1)));
    }
#endif

    /* create nodes */
    for (i = 0; i < length(nodes); i++) {
        whichSubG = INTEGER(subGIndex)[i];
        tmpGraph = whichSubG > 0 ? sgs[whichSubG-1] : g;

        cnode = ALLOC_CHAR(nodes, i);
        curNode = agnode(tmpGraph, cnode);
        R_Free(cnode);

        /*	printf(" node %d in subgraph %d \n", i, whichSubG); */
    }

    /* create the edges */
    char* node_f; char* node_t;
    for (i = 0; i < length(edges_from); i++) {
        node_f = ALLOC_CHAR(nodes, INTEGER(edges_from)[i]-1);
        tail = agfindnode(g, node_f);
        R_Free(node_f);
        if ( !tail ) error("Missing tail node");

        node_t = ALLOC_CHAR(nodes, INTEGER(edges_to)[i]-1);
        head = agfindnode(g, node_t);
        R_Free(node_t);
        if ( !head ) error("Missing head node");

        whichSubG = INTEGER(subGIndex)[INTEGER(edges_from)[i]-1];
        tmpGraph = whichSubG > 0 ? sgs[whichSubG-1] : g;

        /* recipEdges == "combined" in directed graph */
        if ( (ag_k == 1 || ag_k == 3 ) && recip == 0 &&
             (curEdge = agfindedge(tmpGraph, head, tail)) )
        {
            agsafeset(curEdge, "dir", "both", "forward");
        }
        else
        {
            curEdge = agedge(tmpGraph, tail, head);
        }
    }

    return(buildRagraph(g));

}

