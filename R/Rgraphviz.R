agopen <- function(graph,  name, nodes, edges, kind=NULL,
                   layout=TRUE, layoutType=c("dot","neato","twopi")[1],
                   attrs=list(),
                   nodeAttrs=list(), edgeAttrs=list(),
                   subGList=list(), edgeMode=edgemode(graph),
                   recipEdges=c("combined", "distinct")) {


    recipEdges <- match.arg(recipEdges)
    attrs <- getDefaultAttrs(attrs, layoutType)
    checkAttrs(attrs)

    if ((missing(graph)) && (missing(edgeMode)))
        stop("Must pass in either 'graph' or 'edgeMode'")

    if (missing(nodes)) {
        if (missing(graph))
            stop("Must supply either parameter 'graph' or 'nodes'")
        nodes <- buildNodeList(graph, nodeAttrs, subGList)
    }
    if (missing(edges)) {
        if (missing(graph))
            stop("Must supply either parameter 'graph' or 'edges'")
        edges <- buildEdgeList(graph, edgeAttrs, subGList,
                               recipEdges=recipEdges)
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
    if (inherits(graph,"graphNEL"))
        stop("Please use function agopen() for graphNEL objects")
    if (!inherits(graph,"Ragraph"))
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

buildNodeList <- function(graph, nodeAttrs=list(), subGList=list()) {
    pNodes <- list()

    nodeNames <- as.character(nodes(graph))

    if (length(nodeNames) > 0) {
        pNodes <- lapply(nodeNames, function(x) {
            new("pNode",name=x, attrs=list(label=x))})
        names(pNodes) <- nodeNames

        attrNames <- names(nodeAttrs)
        for (i in 1:length(nodeNames)) {
            ## See if this node is in a subgraph
            if (length(subGList) > 0) {
                subGs <- which(unlist(lapply(subGList, function(x,y){y %in% nodes(x)},
                                             nodeNames[i])))
                if (length(subGs) == 1)
                    pNodes[[i]]@subG <- subGs ## FIXME: Need replace method
                else if (length(subGs) > 1)
                    stop("Node ", nodeNames[i], " in multiple subgraphs")
            }
        }

        for (j in seq(along=nodeAttrs)) {
            names <- names(nodeAttrs[[j]])
            for (k in seq(along=nodeAttrs[[j]])) {
                pNodes[[ names[k] ]]@attrs[[ attrNames[j] ]] <-
                    as.character(nodeAttrs[[j]][k])
            }
        }
    }

    pNodes
}


buildEdgeList <- function(graph, recipEdges=c("combined", "distinct"),
                          edgeAttrs=list(), subGList=list()) {
    if (numEdges(graph) == 0)
        return(list())

    recipEdges <- match.arg(recipEdges)

    buildPEList <- function(x, y, weights, edgemode) {
        ## FIXME: Can this assumption be safely made?
        wtNames <- nodes(graph)[names(weights)]
        weights <- as.character(weights)
        names(weights) <- wtNames

        if (edgemode == "directed")
            mapply(function(z, w) {new("pEdge", from=as.character(x),
                                       to=as.character(z),
                                       attrs=list(arrowhead="open",
                                       weight=w))},
                   y, weights)
        else
           mapply(function(z, w) {new("pEdge", from=as.character(x),
                                      to=as.character(z),
                                      attrs=list(arrowhead="none",
                                      weight=w))},
                  y, weights)
    }

    buildSubGEdgeNames <- function(subG) {
        x <- edges(subG)
        z <- names(x)
        as.vector(mapply(paste, x, z, MoreArgs=list(sep="~")))
    }


    edgemode <- edgemode(graph)

    to <- edges(graph)
    if (length(to) == 0)
        return(list())

    from <- names(to)

    pEdges <- mapply(buildPEList, from, to, edgeWeights(graph),
                     MoreArgs=list(edgemode=edgemode))

    ## FIXME: Sometimes the Mapply has a list of lists,
    ## and sometimes just a list of length unlist(edges(graph))
    ## In the former case it needs to be unlisted, the latter
    ## it doesn't.  Why the difference?  Need to sort this out.
    if (! any(unlist(lapply(pEdges, inherits, "pEdge"))))
        pEdges <- unlist(pEdges, recursive=FALSE)

    edgeNames <- edgeNames(graph)
    names(pEdges) <- edgeNames

    subGEdgeNames <- lapply(subGList, buildSubGEdgeNames)

    attrNames <- names(edgeAttrs)
    for (j in seq(along=edgeAttrs)) {
        names <- names(edgeAttrs[[j]])

        for (k in seq(along=edgeAttrs[[j]])) {
            pEdges[[ names[k] ]]@attrs[[ attrNames[j] ]] <-
                as.character(edgeAttrs[[j]][k])
        }
    }

    handled <- character()
    remove <- numeric()
    for (i in 1:length(edgeNames)) {
        revName <- paste(to(pEdges[[i]]), from(pEdges[[i]]),
                         sep="~")

        if ((recipEdges == "distinct") || (! revName %in% handled)) {
            handled <- c(handled, edgeNames[i])
            ## See if this edge is in a subgraph
            if (length(subGList) > 0) {
                subGs <- which(unlist(lapply(subGEdgeNames, function(x,y)
                                         {y %in% x}, edgeNames[i])))
                if (length(subGs) == 1)
                    pEdges[[i]]@subG <- subGs
                else if (length(subGs) == 2)
                    stop("Edge ", edgeNames[i], " is in multiple subgraphs")
            }
        }
        else {
            remove <- c(remove, i)
            if ((is.null(pEdges[[revName]]@attrs$arrowtail))
                &&(edgemode == "directed"))
                pEdges[[revName]]@attrs$arrowtail <- "open"
        }
    }

    if (length(remove) > 0)
        pEdges <- pEdges[-remove]

    pEdges
}


setMethod("edgeNames", "Ragraph", function(object) {
    sapply(AgEdge(object), function(x) paste(tail(x), head(x), sep="~"))
})

