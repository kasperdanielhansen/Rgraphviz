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
        ## Make sure we don't have any NAs in the matrix
        if (!is.numeric(gvMtrx))
            stop("Invalid graph object, produces non-numeric values")
        gvMtrx
    }, where=where)


    setMethod("plot", "graphNEL",
              function(x, y, ..., nodeLabels, centerNode,
                       nodeCols=par("bg"), textCols=par("fg"), edgeCols=par("col")){
                  if (missing(y))
                      y <- "dot"

                  ## Get edgemode of graph
                  edgeMode <- edgemode(x)
                  agKind <- switch(edgeMode,
                                   "undirected"="AGRAPH",
                                   "directed"="AGDIGRAPH",
                                   "AGRAPH")

                  edges <- edges(x)
                  if(missing(nodeLabels) )
                      nodeLabels <- nodes(x)

                  ## Generate an attrs list (currently just
                  ## center)
                  attrs <- vector(length=3,mode="list")
                  names(attrs) <- c("graph","node","edge")

                  ## First make sure that center exists in
                  ## nodeLabels
                  if (!missing(centerNode)) {
                      if ((centerNode %in% nodeLabels)&&
                          (length(centerNode) == 1)){
                          attrs$graph$center <- centerNode
                      }
                      else
                          stop("Invalid center supplied, must be a single node in the graph")
                  }

                  ## twopi layout requires the graph to be fully
                  ## connected
                  g = agopen(x, "ABC", agKind, layout=TRUE,
                             layoutType=y, attrs=attrs)

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
                           yaxt="n",bty="n",...)
                      symbols(nodeX, nodeY, circles=rad, inches=FALSE,
                              bg=nodeCols,add=TRUE)

                      ## Plot the edges
                      ## Need to manually handle the color cycling
                      colEnv <- new.env()
                      colNum <- 1
                      assign("colNum",colNum, colEnv)
                      q <- lapply(AgEdge(g), function(x,cols, colEnv) {
                          colNum <- get("colNum",colEnv)
                          lines(x, col=edgeCols[colNum])
                          if (colNum == length(edgeCols))
                              colNum <- 1
                          else
                              colNum <- colNum + 1
                          assign("colNum",colNum,colEnv)
                      }, edgeCols, colEnv)

                      text(nodeX,nodeY, nodeLabels, col=textCols)
                  }
                  else {
                      stop("No nodes in graph")
                  }
                  invisible(list(nodeLocs=nodeLocs, edges=AgEdge(g),
                              nodeLabels=nodeLabels))
              }, where=where)
}


