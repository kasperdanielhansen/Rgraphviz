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
                       defNodeCol=par("bg"), nodeCols,
                       defTextCol=par("fg"), textCols,
                       defEdgeCol=par("col"),edgeCols=list(),
                       rankDir, fixedSize=TRUE){
                  if (missing(y))
                      y <- "dot"

                  ## Get edgemode of graph
                  edgeMode <- edgemode(x)
                  agKind <- switch(edgeMode,
                                   "undirected"="AGRAPH",
                                   "directed"="AGDIGRAPH",
                                   "AGRAPH")
                  nodes <- nodes(x)
                  edges <- edges(x)
                  if(missing(nodeLabels) )
                      nodeLabels <- nodes

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

                  ## Check on nodeCols.  If it is a named vector, it
                  ## needs to be in the same order as the node names
                  nC <- rep(defNodeCol,length(nodes))
                  names(nC) <- nodes
                  if (!missing(nodeCols))
                      nC[match(names(nodeCols),names(nC))] <- nodeCols

                  ## Now setup the textCols
                  tC <- rep(defTextCol, length(nodes))
                  print(tC)
                  names(tC) <- nodes
                  if (!missing(textCols))
                      tC[match(names(textCols), names(tC))] <- textCols
                  print(tC)
                  nL <- nodeLabels
                  if (fixedSize)
                      nL <- rep(nL[match(max(nchar(nL)), nchar(nL))],
                                length(nodes))

                  g = agopen(x, "ABC", nL, agKind,  layout=TRUE,
                             layoutType=y, attrs=attrs)

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

                      ## Set up the plot region, plot the edges, then
                      ## nodes and finally the node labels
                      mfrow <- par("mfrow")
                      opar <- par(pty="s", oma=c(0,0,0,0),
                                  mai=c(0,0,0,0), mfrow=mfrow)
                      on.exit(par=opar, add=TRUE)
                      plot(NA,NA,xlim=c(0,getX(ur)), ylim=c(0,getY(ur)),
                           type="n",main=NULL,xlab="",ylab="",xaxt="n",
                           yaxt="n",bty="n",...)

                      rad <- switch(nodeShape,
                                   circle=drawCircleNodes(nodeX, nodeY, ur,
                                                         rad, nC),
                                   ellipse=drawEllipseNodes(nodeX, nodeY,
                                                           rad*2, heights, nC)
                                  )


                      ## Plot the edges
                      q <- lapply(AgEdge(g), function(x, edgeCols, defEdgeCol) {
                          ## See if there's a specified edgeCol for this
                          tail <- tail(x)
                          head <- head(x)
                          col <- as.character(edgeCols[[tail]][[head]])
                          if (is.null(col))
                              col <- defEdgeCol
                          lines(x, col=col)
                      }, edgeCols, defEdgeCol)

                      text(nodeX,nodeY, nodeLabels, col=tC)
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
    }
    return(min(widths)/72)
}
