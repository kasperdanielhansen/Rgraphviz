agopen <- function(graph, name, kind=0, layout=TRUE) {
    ### !!! 'kind' is really a set of defined values.  Need to figure
    ### this out

    edges <- edges(graph)
    weights <- edgeWeights(graph)

    g <- .Call("Rgraphviz_agopen", as.character(name), as.integer(kind),
               as.list(edges), as.list(weights), layout)
    return(g)
}

layoutGraph <- function(graph) {
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
    return(graph)
}

graph2ps <- function(graph, name, kind=0, fileName="graph.ps") {
    g <- switch(class(graph),
                "graphNEL"=agopen(graph, name, kind, layout=TRUE),
                "Ragraph"=graph
                )

    return(libgraph2ps(g,fileName))
}

plotGraph <- function(graph, name="graph") {
    ## Really should go into 'graph' package, as a plot method for
    ## the graph class.

    ## Get kind from graph in future
    g = agopen(graph, name, layout=TRUE)

    edges <- edges(graph)
    names <- names(edges)

    if (length(names) > 0) {
        nodes <- nodeLocs(g)

        maxX <- max(nodes[,1])
        rad <- rep(maxX/20, nrow(nodes))

        x <- nodes[,1]
        y <- nodes[,2]

        symbols(x,y,circles=rad, inches=FALSE)
        points(x,y, pch=names, cex=2)
        for (i in 1:length(names)) {
            curEdges <- edges[[i]]
            z <- getLineDiffs(curEdges, i, names, x, y, rad[i])
            lapply(curEdges, plotEdge, i, curEdges, names, x, y, z)
        }
    }
    else {
        stop("No nodes in graph")
    }
    return(g)
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

plotEdge <- function(edge, i, edges, names, x, y, z) {

    ## x1 & y1 are diffs from the tail's x/y coords.
    ## -x1 & -y1 are diffs from the head's x/y coords
    if (is.null(z)) {
        x1 <- rep(0, length(x))
        y1 <- rep(0, length(y))
    }
    else {
        x1 <- z$x1
        y1 <- z$y1
    }

    dimPos <- which(names == edge)
    diffPos <- which(edges == edge)
    ## Code == 2 leaves directional arrowheads
    ## The way this is implemented *currently*, if the link
    ## is undirectional, the line will be drawn in both directions
    ## leaving arrowheads on both sides as desired.
    arrows(x[i]+x1[diffPos], y[i]+y1[diffPos],
           x[dimPos]-x1[diffPos], y[dimPos]-y1[diffPos],code=2)
}


