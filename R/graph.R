agopen <- function(graph, name, kind=0) {
    ### !!! 'kind' is really a set of defined values.  Need to figure
    ### this out

    edges <- edges(graph)
    weights <- edgeWeights(graph)

    g <- .Call("Rgraphviz_agopen", as.character(name), as.integer(kind),
               as.list(edges), as.list(weights))
    return(g)
}

libgraph2ps <- function(graph, fileName="graph.ps") {
    .Call("Rgraphviz_graph2ps",graph,as.character(fileName))
}

graph2ps <- function(graph, name, kind=0, fileName="graph.ps") {
    g <- agopen(graph, name, kind)
    libgraph2ps(g,fileName)
}


##dotLayout <- function(g) {
##    g <- .Call("Rgraphviz_dotLayout", g)
##    return(g)
##}
##
##emitGraph <- function(g, fileName="graph.ps") {
##    .Call("Rgraphviz_emitGraph", g, as.character(fileName))
##}
##
##getDotfile <- function(g) {
    ## Currently only writes to stdout
##    .Call("Rgraphviz_getDotfile", g)
##}

