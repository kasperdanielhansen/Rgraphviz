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
                       xlab="", ylab="", main=NULL, sub=NULL){
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
                                 ylab=ylab, main=main, sub=sub,
                                 newPlot=FALSE))
              })


    setMethod("plot", "Ragraph",
              function(x, y, ...,
                       attrs, xlab="", ylab="", main=NULL, sub=NULL,
                       drawNode=drawAgNode,
                       newPlot=TRUE){

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

                      title(main=main, xlab=xlab, ylab=ylab, sub=sub)

                      if (length(drawNode) == 1)
                         lapply(AgNode(x), drawNode, ur)
                      else if (length(drawNode) == nNodes) {
                          nodes <- AgNode(x)
                          for (i in 1:nNodes) {
                              curDrawFun <- drawNode[[i]]
                              curDrawFun(nodes[[i]], ur)
                          }
                      }
                      else
                          stop("Length of the drawNode parameter",
                               " must be either length 1 or the",
                               " number of nodes.")

                      rad <- min(unlist(lapply(AgNode(x), getNodeRW)))

                      ## Plot the edges
                      q <- lapply(AgEdge(x), function(x, rad,
                                                      edgemode, ur) {
                          ## See if there's a specified edgeCol for this
                          if (!is(x,"AgEdge"))
                              stop(paste("Class:",class("AgEdge")))
                          rad <- convertRadius(rad, ur)
                          tail <- tail(x)
                          head <- head(x)
                          lines(x, len=(rad / 3), edgemode=edgemode)
                      }, rad, edgemode(x), ur)
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

    rw <- getNodeRW(node)
    lw <- getNodeLW(node)
    height <- getNodeHeight(node)

    fg <- color(node)
    bg <- fillcolor(node)

    shape <- shape(node)

    switch(shape,
           "circle"=drawCircleNode(nodeX, nodeY, ur, rw, fg, bg),
           "ellipse"=ellipse(nodeX, nodeY, height=height, width=rw*2,
                             fg=fg, bg=bg),
           "box"=,
           "rect"=,
           "rectangle"=rect(nodeX-lw, nodeY-(height/2), nodeX+rw,
                            nodeY+(height/2), col=bg, border=fg),
           stop("Unimplemented shape"))

    drawTxtLabel(txtLabel(node), nodeX, nodeY)
}

drawTxtLabel <- function(txtLabel, xLoc, yLoc) {
    if ((!is.null(txtLabel))&&(length(labelText(txtLabel)) > 0)) {
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

        text(xLoc, yLoc, labelText(txtLabel), col=labelColor(txtLabel))
    }
}

convertRadius <- function(rad, ur) {
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

    rad/conv
}

drawCircleNode <- function(nodeX, nodeY, ur, rad, fg, bg) {

    rad <- convertRadius(rad, ur)

    invisible(symbols(nodeX, nodeY, circles=rad, inches=max(rad),
                      fg=fg, bg=bg,add=TRUE))
}
