### Methods for the graph classes in package graph

weightLabels <- function(object) {
    if( ! is(object, "graph") )
        stop("need a graph object")
    ## Will return the edge weights of a graph in a format
    ## that is appropriate for use with the edge labels in
    ## a plotted graph

    weights <- edgeWeights(object)

    ## The elements need to be modified such that the names are
    ## the character names of the to nodes, not numeric and
    ## the values in the vector are character - not numeric
    nodes <- nodes(object)

    labels <- lapply(weights, function(x, nodes) {
        labNames <- names(x)
        x <- as.character(x)
        names(x) <- nodes[as.numeric(labNames)]
        x}, nodes)

    labels
}

.initRgraphvizPlotMethods <- function() {

    setMethod("plot", "graph",
              function(x, y, ..., nodeLabels=nodes(x),
                       edgeLabels = list(),
                       nodeCols=character(),
                       textCols=character(),
                       edgeCols=list(),
                       subGList=list(), attrs, xlab="", ylab=""){
                  if (!validGraph(x))
                      stop("The graph to be plotted is not a valid graph structure")
                  if (missing(y))
                      y <- "dot"

                  ## Need to call plot.new before getting the default
                  ## attributes as it uses par("pin") and must be
                  ## on the proper plotting frame if the user is using
                  ## layout.
                  plot.new()

                  if (missing(attrs))
                      attrs <- getDefaultAttrs(y)

                  g <- agopen(x, "ABC", layout=TRUE, layoutType=y,
                              attrs=attrs, subGList=subGList)

                  invisible(plot(g,attrs=attrs, xlab=xlab,
                                 ylab=ylab, nodeCols=nodeCols,
                                 textCols=textCols,
                                 edgeCols=edgeCols,
                                 newPlot=FALSE))
              })


    setMethod("plot", "Ragraph",
              function(x, y, ...,
                       attrs, xlab="", ylab="",
                       nodeCols=character(), textCols=character(),
                       edgeCols=list(), newPlot=TRUE){

                  ## If this is a new plot, we need to call 'plot.new'
                  ## Otherwise we should not because we were most
                  ## likely called from something like plot.graph
                  ## which has already called (and can't avoid)
                  ## calling plot.new().
                  if (newPlot)
                      plot.new()

                  if (missing(attrs))
                      attrs <- getDefaultAttrs(layoutType(x))
                  checkAttrs(attrs)

                  nNodes <- length(nodes(x))

                  if (nNodes > 0) {
                      nodeLocs <- getNodeLocs(x)

                      nodeX <- nodeLocs[["x"]]
                      nodeY <- nodeLocs[["y"]]

                      ## Get the radii of the nodes.  For now we're just
                      ## implementing circles and ellipses
                      rad <- unlist(lapply(nodePos(x), getNodeRW))
                      RWidths <- rad
                      heights <- unlist(lapply(nodePos(x), getNodeHeight))

                      ## Get the upper right X,Y point of the bounding
                      ## box for the graph
                      ur <- upRight(boundBox(x))

                      ## Set up the plot region, plot the edges, then
                      ## nodes and finally the node labels.  We need
                      ## to emulate what happens in 'plot.default' as
                      ## we called plot.new() above, and for the same
                      ## reasons as doing that, calling 'plot' now
                      ## will mung up the thing if people are using
                      ## 'layout' with this.
                      if (missing(xlab))
                          xlab <- ""
                      if (missing(ylab))
                          ylab <- ""

                      ## Setup the node and text color vectors
                      nodeNames <- getNodeNames(x)
                      nC <- getCols(attrs$node$fillcolor, nodeCols,
                                    nodeNames)
                      tC <- getCols(attrs$graph$fontcolor, textCols,
                                    nodeNames)


                      ## !! Currently hardcoding log & asp,
                      ## !! probably want to change that over time.
                      plot.window(xlim=c(0,getX(ur)),
                                  ylim=c(0,getY(ur)),
                                  log="",asp=NA, ...)
                      xy <- xy.coords(NA, NA, xlab, ylab, "")
                      ## !! Also still hardcoding 'type'
                      plot.xy(xy, type="n", ...)

                      rad <- switch(attrs$node$shape,
                                    circle=drawCircleNodes(nodeX, nodeY, ur,
                                    rad, nC),
                                    ellipse=drawEllipseNodes(nodeX, nodeY,
                                    rad*2, heights, nC)
                                    )

                      nodeLabels <- getNodeLabels(x)
                      ## Plot the edges
                      q <- lapply(AgEdge(x), function(x, edgeCols,
                                                      defEdgeCol, rad) {
                          ## See if there's a specified edgeCol for this
                          if (!is(x,"AgEdge"))
                              stop(paste("Class:",class("AgEdge")))
                          tail <- tail(x)
                          head <- head(x)
                          col <- as.character(edgeCols[[tail]][[head]])
                          if (length(col)==0)
                              col <- defEdgeCol
                          lines(x, col=col, len=(rad / 3))
                      }, edgeCols, attrs$edge$color, rad)


                      text(nodeX,nodeY, nodeLabels, col=tC)
                  }
                  else {
                      stop("No nodes in graph")
                  }

                  invisible(list(nodeLocs=nodeLocs,
                                 nodeHeights=heights,
                                 nodeRwidths=RWidths,
                                 edges=AgEdge(x),
                                 nodeLabels=nodeLabels))
              })
}

#### Other functions
drawCircleNodes <- function(nodeX, nodeY, ur, rad, nodeCols) {
    outX <- getX(ur)
    outY <- getY(ur)
    outLim <- max(outY, outX)
    pin <- par("pin")
    if (pin[1] == pin[2]) {
        ## Here we have a square plotting region
        ## probably unecessary check
        conv <- outLim/pin[1]
    }
    else if (outLim == outX) {
        conv <- outX/pin[1]
    }
    else {
        conv <- outY/pin[2]
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
        ellipse(nodeX[i], nodeY[i], heights[i],
                widths[i],bg=nodeCols[i])
    }
    return(min(widths)/72)
}


checkNodeShape <- function(nodeShape) {
    validShapes <- c("ellipse", "circle")
    if (nodeShape %in% validShapes)
        return(TRUE)
    else
        stop(paste("Invalid node shape supplied, must be one of:",
                   paste(validShapes, collapse=", ")))
}

getCols <- function(defCol, cols, names) {
    nC <- rep(defCol,length(names))
    names(nC) <- names
    nC[match(names(cols),names(nC))] <- cols
    nC
}

repEdgeLabels <- function(label, graph) {
    ## Will take a single label and generate an edgeLabel list
    ## that uses that label in ever spot

    if (length(label) != 1)
        stop("repEdgeLabels only for single labels")

    edges <- edges(graph)
    if (length(edges) > 0) {
        for (i in 1:length(edges)) {
            cur <- edges[[i]]
            if (length(cur) > 0) {
                new <- rep(label,length(cur))
                names(new) <- cur
                edges[[i]] <- new
            }
        }
    }

    edges
}
