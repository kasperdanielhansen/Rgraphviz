### Methods for the graph classes in package graph

if (is.null(getGeneric("graph2graphviz")))
    setGeneric("graph2graphviz", function(object,...)
               standardGeneric("graph2graphviz"))

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
})


.initRgraphvizPlotMethods <- function() {

    setMethod("plot", "graphNEL",
              function(x, y, ..., nodeLabels, centerNode, nodeShape,
                       defNodeCol=par("bg"), nodeCols=character(),
                       defTextCol=par("fg"), textCols=character(),
                       defEdgeCol=par("col"),edgeCols=list(),
                       rankDir, fixedSize=TRUE){
                  if (!validGraph(x))
                  stop("The graph to be plotted is not a valid graph structure")

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
                  if(missing(nodeLabels))
                      nodeLabels <- nodes
                  else
                      if (length(nodeLabels) != length(nodes))
                          stop(paste("nodeLabels must be the same",
                                     "length as the number of nodes"))

                  ## Generate an attrs list (currently just
                  ## center)
                  attrs <- vector(length=3,mode="list")
                  names(attrs) <- c("graph","node","edge")

                  ## First make sure that center exists in
                  ## nodeLabels
                  if (!missing(centerNode))
                      attrs$graph$center <- checkCenterNode(centerNode, nodeLabels)

                  ## Check node shapes
                  if (missing(nodeShape))
                      attrs$node$shape <- "circle"
                  else
                      attrs$node$shape <- checkNodeShape(nodeShape)

                  ## If this is a dot layout, check the rankdir
                  if (y == "dot") {
                      if (missing(rankDir))
                          attrs$graph$rankdir <- "TB"
                      else
                          attrs$graph$rankdir <- checkRankDir(rankDir)
                  }

                  ## Setup the node and text color vectors
                  nC <- getCols(defNodeCol, nodeCols, nodes)
                  tC <- getCols(defTextCol, textCols, nodes)

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
                      opar <- par(pty="s", oma=c(0,0,0,0),
                                  mai=c(0,0,0,0))
                      on.exit(par=opar, add=TRUE)
                      plot(NA,NA,xlim=c(0,getX(ur)), ylim=c(0,getY(ur)),
                           type="n",main=NULL,xlab="",ylab="",xaxt="n",
                           yaxt="n",bty="n",...)

                      rad <- switch(attrs$node$shape,
                                    circle=drawCircleNodes(nodeX, nodeY, ur,
                                    rad, nC),
                                    ellipse=drawEllipseNodes(nodeX, nodeY,
                                    rad*2, heights, nC)
                                    )


                      ## Plot the edges
                      q <- lapply(AgEdge(g), function(x, edgeCols,
                                                      defEdgeCol, rad) {
                          ## See if there's a specified edgeCol for this
                          if (!is(x,"AgEdge"))
                              stop(paste("Class:",class("AgEdge")))
                          tail <- tail(x)
                          head <- head(x)
                          col <- as.character(edgeCols[[tail]][[head]])
                          if (length(col)==0)
                              col <- defEdgeCol
                          lines(x, col=col, len=(rad / 4))
                      }, edgeCols, defEdgeCol, rad)

                      text(nodeX,nodeY, nodeLabels, col=tC)
                  }
                  else {
                      stop("No nodes in graph")
                  }

                  invisible(list(nodeLocs=nodeLocs, edges=AgEdge(g),
                                 nodeLabels=nodeLabels))
              })
}

#### Other functions
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

checkNodeShape <- function(nodeShape) {
    validShapes <- c("ellipse", "circle")
    if (nodeShape %in% validShapes)
        return(nodeShape)
    else
        stop(paste("Invalid node shape supplied, must be one of:",
                   paste(validShapes, collapse=", ")))
}

checkCenterNode <- function(centerNode, nodeLabels) {
    if ((centerNode %in% nodeLabels)&&
        (length(centerNode) == 1)){
        return(centerNode)
    }
    else
        stop("Invalid center supplied, must be a single node in the graph")
}

checkRankDir <- function(rankDir) {
    if (!(rankDir %in% c("TB","LR")))
        stop("Invalid rankDir parameter, must be 'TB' or 'LR'!")
    else
        return(rankDir)
}

getCols <- function(defCol, cols, names) {
    nC <- rep(defCol,length(names))
    names(nC) <- names
    nC[match(names(cols),names(nC))] <- cols
    nC
}
