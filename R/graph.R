agopen <- function(graph, name, kind=0) {
    ### !!! 'kind' is really a set of defined values.  Need to figure
    ### this out

    edges <- edges(graph)
    weights <- edgeWeights(graph)

    g <- .Call("Rgraph_agopen", as.character(name), as.integer(kind),
               as.list(edges), as.list(weights))
    return(g)
}

doLayout <- function(g) {
    g <- .Call("Rgraph_doLayout", g)
    return(g)
}

emitGraph <- function(g, fileName="graph.out") {
    .Call("Rgraph_emitGraph", g, as.character(fileName))
}

getDotfile <- function(g) {
    ## Currently only writes to stdout
    .Call("Rgraph_getDotfile", g)
}
