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
              function(x, y, ..., nodeLabels, centerNode, nodeShape,
                       nodeCols=par("bg"), textCols=par("fg"),
                       edgeCols=par("col"), rankDir){
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

                  ## Check node shapes
                  if (missing(nodeShape))
                      nodeShape <- "circle"

                  ## First check to see if it is a valid shape,
                  ## currently only allow "circle" or "ellipse"
                  validShapes <- c("ellipse", "circle")
                  if (nodeShape %in% validShapes)
                      attrs$node$shape <- nodeShape
                  else
                      stop(paste("Invalid node shape supplied, must be one of:",
                                 paste(validShapes, collapse=", ")))

                  ## If this is a dot layout, check the rankdir
                  if (y == "dot") {
                      if (missing(rankDir))
                          rankDir <- "TB"
                      else {
                          if (!(rankDir %in% c("TB","LR")))
                             stop("Invalid rankDir parameter, must be 'TB' or 'LR'!")
                      }
                      attrs$graph$rankdir <- rankDir
                  }

                  ## twopi layout requires the graph to be fully
                  ## connected
                  gc()
                  g = agopen(x, "ABC", nodeLabels, agKind,  layout=TRUE,
                             layoutType=y, attrs=attrs)
                  gc()
                  if (length(nodeLabels) > 0) {
                      nodeLocs <- getNodeLocs(g)

                      nodeX <- nodeLocs[["x"]]
                      nodeY <- nodeLocs[["y"]]

                      ## Get the radii of the nodes.  For now we're just
                      ## implementing circles and ellipses
                      rad <- unlist(lapply(nodes(g), getNodeRW))
                      heights <- unlist(lapply(nodes(g), getNodeHeight))

                      ## Get the upper right X,Y point of the bounding
                      ## box for the graph
                      ur <- upRight(boundBox(g))
                      print(ur)

                      ## Set up the plot region, plot the edges, then
                      ## nodes and finally the node labels
                      opar <- par(pty="s", oma=c(0,0,0,0), mai=c(0,0,0,0))
                      on.exit(par=opar, add=TRUE)


                      plot(NA,NA,xlim=c(0,getX(ur)), ylim=c(0,getY(ur)),
                           type="n",main=NULL,xlab="",ylab="",xaxt="n",
                           yaxt="n",bty="n",...)

                      rad <- switch(nodeShape,
                                   circle=drawCircleNodes(nodeX, nodeY, ur,
                                                         rad, nodeCols),
                                   ellipse=drawEllipseNodes(nodeX, nodeY,
                                                           rad*2, heights, nodeCols)
                                  )


                      ## Plot the edges
                      ## Need to manually handle the color cycling
                      colEnv <- new.env()
                      colNum <- 1
                      assign("colNum",colNum, colEnv)
                      q <- lapply(AgEdge(g), function(x,cols, colEnv) {
                          colNum <- get("colNum",colEnv)
                          lines(x, col=edgeCols[colNum], len=rad/2)
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

drawCircleNodes <- function(nodeX, nodeY, ur, rad, nodeCols) {
    outLim <- max(getY(ur), getX(ur))
    pin <- par("pin")
    if (pin[1] == pin[2]) {
        conv <- outLim/pin[1]
    }
    else if (pin[1] > pin[2]) {
        conv <- getX(ur)/pin[1]
    }
    else {
        conv <- getY(ur)/pin[2]
    }
    rad <- rad/conv

    symbols(nodeX, nodeY, circles=rad, inches=max(rad),
            bg=nodeCols,add=TRUE)

    return(min(rad))
}

drawEllipseNodes <- function(nodeX, nodeY, heights, widths, nodeCols)
{
    ##!!! GET RID OF FOR LOOP
    for (i in 1:length(nodeX)) {
        ellipse(nodeX[i], nodeY[i], heights[i], widths[i])
        print(paste(nodeX[i], nodeY[i], heights[i], widths[i]),sep=", ")
    }
    return(min(widths)/72)
}
