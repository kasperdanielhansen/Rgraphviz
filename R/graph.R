.initRgraphvizMethods <- function(where) {
    if (is.null(getGeneric("graph2graphviz")))
        setGeneric("graph2graphviz", function(object,...)
                   standardGeneric("graph2graphviz"), where=where)

    setMethod("graph2graphviz", "graphNEL", function(object) {
        ## Return a 3 column numeric matrix (from, to, weight)
        nodeNames <- nodes(object)
        ed <- edges(object)
        elem <- sapply(ed, length)
        from <- rep(names(elem), elem)
        from <- as.integer(match(from, nodeNames))

        to <- unlist(ed)
        to <- as.integer(match(to, nodeNames))

        weights <- as.integer(unlist(edgeWeights(object)))
        gvMtrx <- matrix(c(from, to, weights), ncol=3)
        gvMtrx
    }, where=where)


    setMethod("plot", "graphNEL",
              function(x, y, ..., nodeLabels){
                  if (missing(y))
                      y <- "dot"

                  ## Get edgemode of graph
                  edgeMode <- edgemode(x)
                  agKind <- switch(edgeMode,
                                   "undirected"="AGRAPH",
                                   "directed"="AGDIGRAPH",
                                   "AGRAPH")

                  ## twopi layout requires the graph to be fully
                  ## connected
                  g = agopen(x, "ABC", agKind, layout=TRUE, layoutType=y)
                  edges <- edges(x)
                  if(missing(nodeLabels) )
                      nodeLabels <- nodes(x)

                  if (length(nodeLabels) > 0) {
                      nodeLocs <- getNodeLocs(g)

                      nodeX <- nodeLocs[["x"]]
                      nodeY <- nodeLocs[["y"]]

                      ## Get the radii of the nodes.  For now we're just
                      ## implementing circles
                      rad <- unlist(lapply(nodes(g), getNodeRW))
                      print(rad)

                      ## Get the upper right X,Y point of the bounding
                      ## box for the graph
                      ur <- upRight(boundBox(g))

                      ## Set up the plot region, plot the edges, then the nodes,
                      ## and finally the node labels
                      opar <- par(pty="s", oma=c(0,0,0,0), mai=c(0,0,0,0))
                      on.exit(par=opar)
                      outLim <- max(getY(ur), getX(ur))
                      plot(NA,NA,xlim=c(0,outLim), ylim=c(0,outLim),
                           type="n",main=NULL,xlab="",ylab="",xaxt="n",
                           yaxt="n",bty="n",...)
                      symbols(nodeX, nodeY, circles=rad, inches=FALSE,
                              bg="white",add=TRUE)
                      ## Plot the edges
                      q <- lapply(AgEdge(g), lines)
                      text(nodeX,nodeY, nodeLabels)
                  }
                  else {
                      stop("No nodes in graph")
                  }
                  invisible(list(nodeLocs=nodeLocs, edges=AgEdge(g),
                              nodeLabels=nodeLabels))
              }, where=where)
}


