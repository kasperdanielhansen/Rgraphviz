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
              function(x, y, ..., nodeLabels){
                  ## Get kind from graph in future
                  g = agopen(x, "ABC", layout=TRUE)

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
                           yaxt="n",...)
                      # ,bty="n"
                      symbols(nodeX, nodeY, circles=rad, inches=FALSE,
                              bg="white",add=TRUE)
                      q <- lapply(edgePoints(g), plot)
                      text(nodeX,nodeY, nodeLabels)
                  }
                  else {
                      stop("No nodes in graph")
                  }
                  return(list(nodeLocs=nodeLocs, edges=edgePoints(g),
                              nodeLabels=nodeLabels))
              }, where=where)
}


