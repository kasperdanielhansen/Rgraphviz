.initRgraphvizMethods <- function(where) {
    if (is.null(getGeneric("graph2graphviz")))
        setGeneric("graph2graphviz", function(object,...)
                   standardGeneric("graph2graphviz"), where=where)

    setMethod("graph2graphviz", "graphNEL", function(object) {
        ## Return a 3 column integer matrix (from, to, weight)
        nodeNames <- nodes(object)
        ed <- edges(object)
        elem <- sapply(ed, length)
        from <- rep(names(elem), elem)
        from <- match(from, nodeNames)

        to <- unlist(ed)
        to <- match(to, nodeNames)

        weights <- unlist(edgeWeights(object))
        gvMtrx <- matrix(c(from, to, weights), ncol=3)
        gvMtrx
    }, where=where)

    setMethod("plot", c("graphNEL", "missing"),
              function(x,y,...){
                  ## Get kind from graph in future
                  g = agopen(x, name, layout=TRUE)

                  edges <- edges(x)
                  names <- names(edges)

                  if (length(names) > 0) {
                      nodeLocs <- getNodeLocs(g)

                      nodeX <- nodeLocs[["x"]]
                      nodeY <- nodeLocs[["y"]]

                      ## Get the radii of the nodes.  FOr now we're just
                      ## implementing circles
                      rad <- unlist(lapply(nodes(g), getNodeRW))

                      ## Get the upper right X,Y point of the bounding
                      ## box for the graph
                      ur <- upRight(boundBox(g))

                      ## Set up the plot region, plot the edges, then the nodes,
                      ## and finally the node labels
                      par(pty="s")
                      outLim <- max(getY(ur), getX(ur))
                      plot(NA,NA,xlim=c(0,outLim), ylim=c(0,outLim),
                           type="n",main=NULL,xlab="",ylab="",xaxt="n",
                           yaxt="n")
                      q <- lapply(edgePoints(g), plot)
                      symbols(nodeX, nodeY, circles=rad, inches=FALSE,
                              bg="white",add=TRUE)

                      text(nodeX,nodeY, names, cex=2)
                  }
                  else {
                      stop("No nodes in graph")
                  }
              }, where=where)
}


