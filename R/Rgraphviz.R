agopen <- function(graph,  name, nodes, edges, kind=NULL,
                   layout=TRUE, layoutType=c("dot","neato","twopi"),
                   attrs=list(),
                   nodeAttrs=list(), edgeAttrs=list(),
                   subGList=list(), edgeMode=edgemode(graph),
                   recipEdges=c("combined", "distinct")) {

    layoutType <- match.arg(layoutType)
    recipEdges <- match.arg(recipEdges)
    attrs <- getDefaultAttrs(attrs, layoutType)
    checkAttrs(attrs)

    if ((missing(graph)) && (missing(edgeMode)))
        stop("Must pass in either 'graph' or 'edgeMode'")

    if (missing(nodes)) {
        if (missing(graph))
            stop("Must supply either parameter 'graph' or 'nodes'")
        nodes <- buildNodeList(graph, nodeAttrs, subGList, attrs$node)
    }
    if (missing(edges)) {
        if (missing(graph))
            stop("Must supply either parameter 'graph' or 'edges'")
        edges <- buildEdgeList(graph, recipEdges, edgeAttrs,  subGList,
                               attrs$edge)
    }

    if (length(subGList) > 0)
        subGs <- paste("cluster_",1:length(subGList), sep="")
    else
        subGs <- character()

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

    ## all attrs must be character strings going into C,
    ## graphviz wants all attrs to be char*
    ## FIXME: so shouldn't we do that in C?
    attrs <- lapply(attrs, function(x){lapply(x,as.character)})

    g <- .Call("Rgraphviz_agopen", as.character(name),
               as.integer(outK), as.list(nodes),
               as.list(edges), as.list(attrs),
               as.character(subGs), PACKAGE="Rgraphviz")
    g@layoutType <- layoutType
    g@edgemode <- edgeMode

    if (layout)
        return(layoutGraph(g))
    else
        return(g)
}

agread <- function(filename, layoutType=c("dot","neato","twopi")[1],
                   layout=TRUE) {
    ## First check that the file exists
    if (!file.exists(filename))
        stop(paste("Request file",filename,"does not exist"))

    g <- .Call("Rgraphviz_agread", as.character(filename), PACKAGE="Rgraphviz")

    if (layout)
        return(layoutGraph(g,layoutType))
    else
        return(g)
}

getGraphAttr <- function(graph, attr) {
    .Call("Rgraphviz_getAttr", graph, as.character(attr),
          PACKAGE="Rgraphviz")
}

agwrite <- function(graph, filename) {
    g <- .Call("Rgraphviz_agwrite", graph, as.character(filename),
               PACKAGE="Rgraphviz")
}

layoutGraph <- function(graph) {
    if (is(graph,"graphNEL"))
        stop("Please use function agopen() for graphNEL objects")
    if (!is(graph,"Ragraph"))
        stop("Object is not of class Ragraph")

    type <- switch(layoutType(graph),
                   "dot"=0,
                   "neato"=1,
                   "twopi"=2,
                   stop(paste("Invalid layout type:",layoutType))
                   )

    if (laidout(graph) == FALSE) {
        z <- .Call("Rgraphviz_doLayout", graph, as.integer(type),
                   PACKAGE="Rgraphviz");
        return(z)
    }
    else {
        return(graph)
    }
}

graphvizVersion <- function() {
    z <- .Call("Rgraphviz_graphvizVersion", PACKAGE="Rgraphviz")
    z
}

buildNodeList <- function(graph, nodeAttrs=list(), subGList=list(),
                          defAttrs=list()) {
    pNodes <- .Call("Rgraphviz_buildNodeList", graph, nodeAttrs, subGList,
                   defAttrs, PACKAGE="Rgraphviz")
}


buildEdgeList <- function(graph, recipEdges=c("combined", "distinct"),
                          edgeAttrs=list(), subGList=list(), defAttrs=list()) {

    recipEdges <- match.arg(recipEdges)

    edgeNames <- edgeNames(graph, "distinct")

    if ((recipEdges == "combined")&&(length(edgeNames) > 0))
        removed <- which(! edgeNames %in% edgeNames(graph, "combined"))
    else
        removed <- character()

    ## Generate the list of pEdge objects
    .Call("Rgraphviz_buildEdgeList", graph, subGList, edgeNames,
          removed, edgeAttrs, defAttrs, PACKAGE="Rgraphviz")
}

setMethod("edgeNames", "Ragraph", function(object,
                                           recipEdges=c("combined",
                                           "distinct")) {
    recipEdges <- match.arg(recipEdges)

    edgeNames <- sapply(AgEdge(object), function(x) paste(tail(x), head(x), sep="~"))

    if (recipEdges == "combined") {
        revNames <- sapply(AgEdge(object), function(x) paste(head(x),
                                                             tail(x), sep="~"))

        handled <- character()
        remove <- numeric()
        for (i in 1:length(edgeNames)) {
            if ((recipEdges == "distinct") || (! revNames[i] %in% handled))
                handled <- c(handled, edgeNames[i])
            else
                remove <- c(remove, i)
        }
        if (length(remove) > 0)
            edgeNames <- edgeNames[-remove]
    }
    edgeNames
})


removedEdges <- function(graph) {
    if ((!is(graph, "graph")) && (!is(graph,"Ragraph")))
        stop("removedEdges only accepts objects of class ",
             "'graph' or 'Ragraph'")

    allEdges <- edgeNames(graph, "distinct")
    combEdges <- edgeNames(graph, "combined")
    which(! allEdges %in% combEdges)
}
