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
    if (numEdges(graph) == 0)
        return(list())

    recipEdges <- match.arg(recipEdges)

    buildSubGEdgeNames <- function(subG) {
        x <- edges(subG)
        z <- names(x)
        as.vector(mapply(paste, x, z, MoreArgs=list(sep="~")))
    }

    edgeNames <- edgeNames(graph, "distinct")

    edgemode <- edgemode(graph)

    to <- edges(graph)
    if (length(to) == 0)
        return(list())

    ## Generate the list of pEdge objects
    pEdges <- .Call("Rgraphviz_buildPEList", to, edgemode, edgeWeights(graph),
                    length(unlist(edges(graph))), PACKAGE="Rgraphviz")

    ## Even if recipEdges is 'combined', it saves a lot of work
    ## when trying to figure out the arrowtail stuff if all of
    ## the pEdge objects are named.
    names(pEdges) <- edgeNames

    if (recipEdges == "combined") {
        removedEdges <- removedEdges(graph)
        if (length(removedEdges) > 0) {
            edgeNames <- edgeNames[-removedEdges]
            if (edgemode == "directed") {
                ## Add in the arrowtails if this was a directed graph
                for (i in 1:length(removedEdges)) {
                    rf <- from(pEdges[[removedEdges[i]]])
                    rt <- to(pEdges[[removedEdges[i]]])
                    revName <- paste(rt, rf, sep="~")
                    if (is.null(pEdges[[revName]]@attrs$arrowtail))
                        pEdges[[revName]]@attrs$arrowtail <- "open"
                }
            }
            pEdges <- pEdges[-removedEdges]
        }
    }

    pEdges <- assignAttrs(edgeAttrs, pEdges, defAttrs)

    if (length(subGList) > 0) {
        subGEdgeNames <- lapply(subGList, buildSubGEdgeNames)
        pEdges <- assignEdgeSubGs(subGEdgeNames, pEdges, edgeNames)
    }

    pEdges
}

assignAttrs <- function(attrList, objList, defAttrs) {
    if (length(attrList) == 0)
        return(objList)

    .Call("Rgraphviz_assignAttrs", attrList, objList, defAttrs, PACKAGE="Rgraphviz")
}

assignEdgeSubGs <- function(subGList, objList, objNames) {

    subGs <- sapply(subGList,
                    function(x, y) {y %in% x}, objNames)

    whichSubGs <- apply(subGs, 1, function(x) {
        out <- which(x)
        if (length(out) == 0)
            0
        else
            out})

    if (is.list(whichSubGs)) {
        bad <- which(sapply(whichSubGs, length) > 1)
        stop(objNames[bad], " is in multiple subgraphs")
    }
    else {
        ## FIXME: Why does this need as.integer and not the nodes?
        objList <- mapply(function(x,y) { x@subG <- as.integer(y); x},
                         objList, whichSubGs)
    }

    objList
}

removedEdges <- function(graph) {
    if ((!is(graph, "graph")) && (!is(graph,"Ragraph")))
        stop("removedEdges only accepts objects of class ",
             "'graph' or 'Ragraph'")

    allEdges <- edgeNames(graph, "distinct")
    combEdges <- edgeNames(graph, "combined")
    which(! allEdges %in% combEdges)
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


