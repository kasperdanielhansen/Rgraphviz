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

    ## FIXME: For now, in graphviz 2.4 on a neato graph w/
    ##  singleton nodes, it will segfault.  The root cause
    ##  of this is in Graphviz proper and has been fixed in
    ##  the 2.5 devel branch (and thus 2.6).  Try and work
    ##  around this in a less hassling manner.
    if ((graphvizVersion() == "2.4")&&(layoutType == "neato")) {
        singletonGraph <- any(sapply(connComp(graph), function(x)
                                     length(x) <= 1))
        if (singletonGraph == TRUE)
            stop("There is a bad interaction between ",
                 "Rgraphviz and Graphviz 2.4 involving ",
                 "graphs with singleton nodes laid out with neato.\n",
                 "Hopefully we can find a workaround for ",
                 "this situation, but until then you can ",
                 "use Graphviz versions earlier or later than 2.4.")
    }


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

    if (layout)
        return(layoutGraph(g))
    else
        return(g)
}

agread <- function(filename, layoutType=c("dot","neato","twopi"),
                   layout=TRUE) {
    layoutType <- match.arg(layoutType)
    ## First check that the file exists
    if (!file.exists(filename))
        stop(paste("Request file",filename,"does not exist"))

    g <- .Call("Rgraphviz_agread", as.character(filename), PACKAGE="Rgraphviz")
    g@layoutType <- layoutType

    if (layout)
        return(layoutGraph(g))
    else
        return(g)
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
    .Call("Rgraphviz_buildNodeList", nodes(graph), nodeAttrs,
                          subGList, defAttrs, PACKAGE="Rgraphviz")
}

#Katz, May 2005, more fixes to come later
fne <- function(el) {
    ## If there's some sort of empty element,
    ## return a list with an empty numeric vector
    ## so as to properly work w/ the C code
    if (length(el) == 0)
        list(edges=numeric())
    else
        el
}

buildEdgeList <- function(graph, recipEdges=c("combined", "distinct"),
                          edgeAttrs=list(), subGList=list(), defAttrs=list()) {
    recipEdges <- match.arg(recipEdges)

    edgeNames <- edgeNames(graph, "distinct")

    if ((recipEdges == "combined")&&(length(edgeNames) > 0))
        removed <- which(! edgeNames %in% edgeNames(graph, "combined"))
    else
        removed <- character()

    aa <- edgeL(graph)
    aa<-lapply(aa,fne)

    ## Generate the list of pEdge objects
    .Call("Rgraphviz_buildEdgeList", aa, edgemode(graph),
          subGList, edgeNames, removed, edgeAttrs, defAttrs,
          PACKAGE="Rgraphviz")
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

setMethod("edgeL", "clusterGraph", function(graph, index) {
    ## temporary function, just a placeholder until I put in
    ## something better (most likely in C)

    clusters <- connComp(graph)
    nodes <- nodes(graph)
    edgeL <- list()

    cur <- 1
    for (i in seq(along=clusters)) {
        curClust <- clusters[[i]]
        for (j in seq(along = curClust)) {
            edgeL[[cur]] <- list(edges=match(curClust[-j], nodes))
            cur <- cur + 1
        }
    }
    names(edgeL) <- nodes

    if (! missing(index))
        edgeL <- edgeL[[index]]

    edgeL
})

setMethod("edgeL", "distGraph", function(graph, index) {
    ## Just like the clusterGraph edgeL method, this should be
    ## considered a poor place holder for now.
    edges <- edges(graph)
    edgeL <- mapply(function(x, y, nodes) {
        out <- list(edges=match(x, nodes), weights=y)
    }, edges, edgeWeights(graph), MoreArgs=list(nodes=nodes(graph)),
                    SIMPLIFY=FALSE)
    names(edgeL) <- names(edges)

    if (! missing(index))
        edgeL <- edgeL[[index]]

    edgeL
})


if (is.null(getGeneric("toDot")))
    setGeneric("toDot", function(graph, filename, ...)
               standardGeneric("toDot"))

setMethod("toDot", "graph", function(graph, filename, ...) {
    z <- agopen(graph, name = "foo", ...)
    agwrite(z, filename)
})


