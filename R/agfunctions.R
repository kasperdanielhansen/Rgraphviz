agopen <- function(graph,  name, nodes, edges, kind=NULL,
                   layout=TRUE,	layoutType="dot", 
                   attrs=list(), nodeAttrs=list(), edgeAttrs=list(),
                   subGList=list(), edgeMode=edgemode(graph),
                   recipEdges=c("combined", "distinct")) {

    if ( missing(graph) )
    {
       if ( missing(nodes) || missing(edges) || missing(edgeMode) )
          stop("When not providing graph, need to provide ",
               "'nodes', 'edges' and 'edgeMode'.")
    }
    else
    {
       if (!is(graph,"graph"))
          stop("graph is not an object of class 'graph'")
    }

    recipEdges <- match.arg(recipEdges)
    layoutType <- .checkLayoutType(layoutType)
    attrs <- getDefaultAttrs(attrs, layoutType)
    checkAttrs(attrs)

    if (missing(nodes)) {
        nodes <- buildNodeList(graph, nodeAttrs, subGList, attrs$node)
    }
    if (missing(edges)) {
        edges <- buildEdgeList(graph, recipEdges, edgeAttrs,  subGList, attrs$edge)
    }

    if (is.null(kind)) {
        ## Determine kind from the graph object
        outK <- switch(edgeMode,
                       "undirected"=0,  ## AGRAPH
                       "directed"=1,    ## AGDIGRAPH
                       0)
    }
    else {
        ## Use the specified 'kind' parameter.
        outK <- switch(kind,
                       "AGRAPH"=0,   ##Undirected Graph
                       "AGDIGRAPH"=1,   ## directed graph
                       "AGRAPHSTRICT"=2,   ## no self arcs or multiedges
                       "AGDIGRAPHSTRICT"=3, ## directed strict graph
                       stop(paste("Incorrect kind parameter:",kind)))
    }

    g <- .Call("Rgraphviz_agopen", as.character(name),
               as.integer(outK), as.list(nodes),
               as.list(edges), as.list(attrs),
               as.list(subGList), PACKAGE="Rgraphviz")
    g@layoutType <- layoutType
    g@edgemode <- edgeMode

    ## Allows lwd (line width) and lty (line type) to be set in same manner
    ## color is set
    ## FIXME:  fix examples in AgNode/AgEdge
    ##       shouldn't do graphLayout here, but AgEdge/AgNode count on it
    ##       to fill in data entries, hence, examples from man-pages...
    ##
    if (layout) g <- graphLayout(g)

    if (!is.null(edgeAttrs$lwd)) {
        for (i in seq(along=edgeAttrs$lwd)) {
            attr(attr(g, "AgEdge")[[i]], "lwd") <- edgeAttrs$lwd[i]
        }
    }

    if (!is.null(edgeAttrs$lty)) {
        for (i in seq(along=edgeAttrs$lty)) {
            attr(attr(g, "AgEdge")[[i]], "lty") <- edgeAttrs$lty[i]
        }
    }

    return(g)
}

# graph: graphNEL
# name: string
# kind: int
# subGList: list of subgraphs
# recipEdges: combine reciprocal directed edges or not 
agopenSimple <- function(graph, name, 
		kind=NULL, edgeMode=edgemode(graph),
                subGList=list(), 
                recipEdges=c("combined", "distinct")) 
{
    if (!is(graph,"graph"))
        stop("This function is for objects of class 'graph' only")

    if (is.null(kind)) {
        ## Determine kind from the graph object
        outK <- switch(edgeMode,
                       "undirected"=0,  ## AGRAPH
                       "directed"=1,    ## AGDIGRAPH
                       0)
    }
    else {
        ## Use the specified 'kind' parameter.
        outK <- switch(kind,
                       "AGRAPH"=0,   ##Undirected Graph
                       "AGDIGRAPH"=1,   ## directed graph
                       "AGRAPHSTRICT"=2,   ## no self arcs or multiedges
                       "AGDIGRAPHSTRICT"=3, ## directed strict graph
                       stop(paste("Incorrect kind parameter:",kind)))
    }

    recipEdges <- match.arg(recipEdges)
    recipK <- switch(recipEdges,
                   "combined"=0,  
                   "distinct"=1, 
                    0)

    nsubG = length(subGList)

    sgi = vector(mode="numeric", length=numNodes(graph))
    sgi[] = nsubG 
    names(sgi) = nodes(graph)
    if ( nsubG > 0 ) 
	for ( i in 1:nsubG ) sgi[nodes(subGList[[i]]$graph)] = i

    g <- .Call("Rgraphviz_agopenSimple", name, as.integer(outK), 
		nodes(graph), as.integer(sgi),
		as.integer(edgeMatrix(graph)["from",]), 
		as.integer(edgeMatrix(graph)["to",]), 
		as.integer(nsubG), as.list(subGList),
		as.integer(recipK), 
                PACKAGE="Rgraphviz")
    g@edgemode <- edgeMode
    g@layoutType = "dot"    # default layout type

    g
}

agread <- function(filename, layoutType = "dot",
                   layout=TRUE	)
{
    layoutType <- .checkLayoutType(layoutType)
    ## First check that the file exists
    if (!file.exists(filename))
        stop(paste("Request file",filename,"does not exist"))

    g <- .Call("Rgraphviz_agread", as.character(filename), PACKAGE="Rgraphviz")
    g@layoutType <- layoutType

    ## FIXME:
    ##       shouldn't do graphLayout here, but AgEdge/AgNode count on it
    ##       to fill in data entries, hence, examples from man-pages...
    ##
    if (layout)
        return(graphLayout(g))
    else
        return(g)
}

