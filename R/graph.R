## FIXME: Can graph2graphviz & weightLabels go?
graph2graphviz <- function(object) {
    if( ! is(object, "graph") )
        stop("need a graph object")
    ## Return a 3 column numeric matrix (from, to, weight)
    fromTo <- edgeMatrix(object, duplicates=TRUE)
    colnames(fromTo) <- NULL
    weights <- unlist(edgeWeights(object))

    gvMtrx <- rbind(fromTo, weights)
    ## Make sure the matrix is numeric
    if (!is.numeric(gvMtrx))
        stop("non-numeric values in the edge matrix")
    if (any(is.na(gvMtrx)))
        stop("NAs in the edge matrix")

    gvMtrx
}

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
              function(x, y, ..., subGList=list(),
                       attrs=getDefaultAttrs(y),
                       nodeAttrs=list(), edgeAttrs=list(),
                       xlab="", ylab=""){
                  if (!validGraph(x))
                      stop("The graph to be plotted is not a valid graph structure")
                  if (missing(y))
                      y <- "dot"

                  ## Need to call plot.new before getting the default
                  ## attributes as it uses par("pin") and must be
                  ## on the proper plotting frame if the user is using
                  ## layout.
                  plot.new()

                  g <- agopen(x, "ABC", layout=TRUE, layoutType=y,
                              attrs=attrs, nodeAttrs=nodeAttrs,
                              edgeAttrs=edgeAttrs, subGList=subGList)

                  invisible(plot(g,attrs=attrs, xlab=xlab,
                                 ylab=ylab, newPlot=FALSE))
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

                  nNodes <- length(AgNode(x))

                  if (nNodes > 0) {
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

                      ## !! Currently hardcoding log & asp,
                      ## !! probably want to change that over time.
                      plot.window(xlim=c(0,getX(ur)),
                                  ylim=c(0,getY(ur)),
                                  log="",asp=NA, ...)
                      xy <- xy.coords(NA, NA, xlab, ylab, "")
                      ## !! Also still hardcoding 'type'
                      plot.xy(xy, type="n", ...)

                      rad <- min(unlist(lapply(AgNode(x), drawAgNode, ur)))

                      ## Plot the edges
                      q <- lapply(AgEdge(x), function(x, edgeCols,
                                                      defEdgeCol, rad,
                                                      edgemode) {
                          ## See if there's a specified edgeCol for this
                          if (!is(x,"AgEdge"))
                              stop(paste("Class:",class("AgEdge")))
                          tail <- tail(x)
                          head <- head(x)
                          col <- as.character(edgeCols[[tail]][[head]])
                          if (length(col)==0)
                              col <- defEdgeCol
                          lines(x, col=col, len=(rad / 3),
                                edgemode=edgemode)
                      }, edgeCols, attrs$edge$color, rad, edgemode(x))
                  }
                  else {
                      stop("No nodes in graph")
                  }

                  invisible(x)
              })
}

drawAgNode <- function(node, ur) {

    ## First get X/Y
    nodeCenter <- getNodeCenter(node)
    nodeX <- getX(nodeCenter)
    nodeY <- getY(nodeCenter)

    rad <- getNodeRW(node)
    height <- getNodeHeight(node)

    fg <- color(node)
    bg <- fillcolor(node)

    shape <- shape(node)

    out <- switch(shape,
                  "circle"=drawCircleNode(nodeX, nodeY, ur, rad, fg, bg),
                  "ellipse"=ellipse(nodeX, nodeY, height=height, width=rad*2,
                  fg=fg, bg=bg),
                  stop("Unimplemented shape"))

    drawTxtLabel(txtLabel(node), nodeX, nodeY)
    out
}

drawTxtLabel <- function(txtLabel, xLoc, yLoc) {
    if (!is.null(txtLabel)) {
        loc <- labelLoc(txtLabel)
        if (missing(xLoc)) {
            justMod <- switch(labelJust(txtLabel),
                              "l" = 0,
                              "n" = -0.5,
                              "r" = -1)

            xLoc <- getX(loc) + (justMod * labelWidth(txtLabel))
        }
        if (missing(yLoc))
            yLoc <- getY(loc)

        text(xLoc, yLoc, labelText(txtLabel))
    }
}


drawCircleNode <- function(nodeX, nodeY, ur, rad, fg, bg) {
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
            fg=fg, bg=bg,add=TRUE)
    rad
}

