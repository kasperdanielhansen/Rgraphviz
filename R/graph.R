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

plotGraph <- function(graph, name="graph") {
    ## Really should go into 'graph' package, as a plot method for
    ## the graph class.

    ## Get kind from graph in future
    g = agopen(graph, name, layout=TRUE)

    edges <- edges(graph)
    names <- names(edges)

    if (length(names) > 0) {
        nodeLocs <- getNodeLocs(g)

        nodeX <- nodeLocs[["x"]]
        nodeY <- nodeLocs[["y"]]

        ### !!! For now just drawing circles, and thus only need one
        ### !!! of the half widths to get the radius
        rad <- unlist(lapply(nodes(g), getNodeRW))

        symbols(nodeX,nodeY,circles=rad, inches=FALSE)
        points(nodeX,nodeY, pch=names, cex=2)
        q <- lapply(edgePoints(g), plotEdge)
    }
    else {
        stop("No nodes in graph")
    }
}

plotEdge <- function(edgePnt) {
    z <- splines(edgePnt)
    lapply(z,function(x){q <- bezierPoints(x); lines(q[,1],q[,2])})
    return(NULL)
}

getLineDiffs <- function(edges, i, names, x, y, rad) {
    pos <- match(edges, names)
    if (length(pos) == 0)
        return(NULL)

    tailX <- x[pos]
    tailY <- y[pos]

    X <- tailX - x[i]
    Y <- tailY - y[i]

    h <- sqrt((X * X) + (Y * Y))

    ## These are diffs off of the main X/Y for
    ## tail node
    x1 <- (rad * X)/h
    y1 <- (rad * Y)/h

    return(list(x1=x1,y1=y1))
}

bezier <- function(pnts, n, t) {
    n <- n-1
    x <- 0
    for (k in 0:n) {
        x <- x + (pnts[[k+1]] * choose(n,k) * (t^k) * ((1-t)^(n-k)))
    }
    return(x)
}
