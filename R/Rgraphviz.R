agopen <- function(graph, name, kind=0, layout=TRUE) {
    ### !!! 'kind' is really a set of defined values.  Need to figure
    ### this out

    edges <- uniqueEdges(graph)
    nodes <- nodes(graph)
    weights <- edgeWeights(graph)

    g <- .Call("Rgraphviz_agopen", as.character(name), as.integer(kind),
               as.vector(nodes), as.list(edges), as.list(weights))

    if (layout)
        return(layoutGraph(g))
    else
        return(g)
}

layoutGraph <- function(graph) {
    if (inherits(graph,"graphNEL"))
        stop("Please use function agopen() for graphNEL objects")
    if (!inherits(graph,"Ragraph"))
        stop("Object is not of class Ragraph")

    if (laidout(graph) == FALSE) {
        z <- .Call("Rgraphviz_doDotLayout", graph);
        return(z)
    }
    else {
        return(graph)
    }
}

libgraph2ps <- function(graph, fileName="graph.ps") {
    if (laidout(graph) == FALSE)
        graph <- layoutGraph(graph)
    x <- .Call("Rgraphviz_graph2ps",graph,as.character(fileName))
}

graph2ps <- function(graph, name, kind=0, fileName="graph.ps") {
    g <- switch(class(graph),
                "graphNEL"=agopen(graph, name, kind, layout=TRUE),
                "Ragraph"=graph
                )
    libgraph2ps(g,fileName)
}
