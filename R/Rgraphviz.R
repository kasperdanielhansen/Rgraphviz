  agopen <- function(graph, name, kind="AGRAPH", layout=TRUE,
                   layoutType=c("dot","neato","twopi")[1],
                   attrs=NULL) {

      ## graph must be fully connected if twopi
      if ((layoutType=="twopi")&&(isConnected(graph) == FALSE))
          stop("Graph must be fully connected to perform a twopi layout")

      outK <- switch(kind,
                   "AGRAPH"=0,
                   "AGDIGRAPH"=1,
                   "AGRAPHSTRICT"=2,
                   "AGDIGRAPHSTRICT"=3,
                   stop(paste("Incorrect kind parameter:",kind)))

    edgeMtrx <- graph2graphviz(graph)

    nodes <- nodes(graph)

    g <- .Call("Rgraphviz_agopen", as.character(name),
               as.integer(outK), as.vector(nodes),
               as.integer(edgeMtrx[,1]), as.integer(edgeMtrx[,2]),
               as.integer(edgeMtrx[,3]))

    if (is.list(attrs))
        g <- agset(g, attrs)

    if (layout)
        return(layoutGraph(g,layoutType))
    else
        return(g)
}

agset <- function(graph, attrs) {
    if (!is.list(attrs))
        stop("Malformed attrs argument, must be a list")
    if (is(graph,"graphNEL"))
        stop("Please use function agopen() for graphNEL objects")
    if (laidout(graph) == TRUE)
        stop("Graph is already laid out")
    if (!is(graph,"Ragraph"))
        stop("Object is not of class Ragraph")

    g <- .Call("Rgraphviz_agset", graph, as.list(attrs))
    return(g)
}

layoutGraph <- function(graph, layoutType=c("dot","neato","twopi")[1]) {
    if (inherits(graph,"graphNEL"))
        stop("Please use function agopen() for graphNEL objects")
    if (!inherits(graph,"Ragraph"))
        stop("Object is not of class Ragraph")

    type <- switch(layoutType,
                   "dot"=0,
                   "neato"=1,
                   "twopi"=2,
                   stop(paste("Invalid layout type:",layoutType))
                   )

    if (laidout(graph) == FALSE) {
        z <- .Call("Rgraphviz_doLayout", graph, as.integer(type));
        return(z)
    }
    else {
        return(graph)
    }
}
