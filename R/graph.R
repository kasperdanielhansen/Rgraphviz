.initRgraphvizPlotMethods <- function() {
    setMethod("plot", "graph",
              function(x, y, ..., subGList=list(),
                       attrs=list(),
                       nodeAttrs=list(), edgeAttrs=list(),
                       xlab="", ylab="", main=NULL, sub=NULL,
                       recipEdges=c("combined", "distinct")){
                  if (!validGraph(x))
                      stop("The graph to be plotted is not a valid graph structure")
                  if (missing(y))
                      y <- "dot"

                  recipEdges <- match.arg(recipEdges)


                  ## Need to call plot.new before getting the default
                  ## attributes as it uses par("pin") and must be
                  ## on the proper plotting frame if the user is using
                  ## layout.

                  if (! is.null(attrs$graph$size)) {
                      splitSize <- strsplit(attrs$graph$size,",")[[1]]
                      if (length(splitSize) != 2)
                          stop("Invalid graph size attribute: ",
                               attrs$graph$size)
                      wd <- as.numeric(splitSize[1])
                      ht <- as.numeric(splitSize[2])

                      if (interactive())
                          x11(width=wd, height=ht)
                  }

                  plot.new()

                  g <- agopen(x, "ABC", layout=TRUE, layoutType=y,
                              attrs=attrs, nodeAttrs=nodeAttrs,
                              edgeAttrs=edgeAttrs, subGList=subGList,
                              recipEdges=recipEdges)

                  invisible(plot(g,attrs=attrs, xlab=xlab,
                                 ylab=ylab, main=main, sub=sub,
                                 newPlot=FALSE))
              })


    setMethod("plot", "Ragraph",
              function(x, y, ...,
                       attrs=list(), xlab="", ylab="", main=NULL, sub=NULL,
                       drawNode=drawAgNode,
                       newPlot=TRUE){

                  recipEdges=c("combined", "distinct")
                  ## If this is a new plot, we need to call 'plot.new'
                  ## Otherwise we should not because we were most
                  ## likely called from something like plot.graph
                  ## which has already called (and can't avoid)
                  ## calling plot.new().
                  if (newPlot) {
                      sz <- getGraphSize(x)
                      if (interactive())
                          x11(width=sz[1], height=sz[2])

                      plot.new()
                  }

                  attrs <- getDefaultAttrs(attrs, layoutType(x))
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
                          }                      }
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

    rad <- convertRadius(rw, ur)

    fg <- color(node)
    bg <- fillcolor(node)

    shape <- shape(node)

    switch(shape,
           "circle"=drawCircleNode(nodeX, nodeY, ur, rad, fg, bg),
           "ellipse"=ellipse(nodeX, nodeY, height=height, width=rw*2,
                             fg=fg, bg=bg),
           "box"=,
           "rect"=,
           "rectangle"=rect(nodeX-lw, nodeY-(height/2), nodeX+rw,
                            nodeY+(height/2), col=bg, border=fg),
           stop("Unimplemented node shape"))

    drawTxtLabel(txtLabel(node), nodeX, nodeY, width=rad*2)
}

drawTxtLabel <- function(txtLabel, xLoc, yLoc, width) {
    if ((!is.null(txtLabel))&&(length(labelText(txtLabel)) > 0)) {

        txt <- labelText(txtLabel)
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

        ## Get the font size that Graphviz believes this to be
        op <- par()

        curFontsize <- labelFontsize(txtLabel)
        if (length(curFontsize) > 0) {
            on.exit(par(ps=op$ps), add=TRUE)
            par(ps=curFontsize)
        }
        else
            curFontsize <- op$ps

        cex <- op$cex

        if (!missing(width)) {
            ## FIXME:
            ## Due to scaling down to the specified size, the labels
            ## also need to be scaled.  Graphviz does this post-layout,
            ## so we need to as well.  Not sure how best to go about this,
            ## but for now using this bad hack -> basically the idea
            ## is to keep trying smaller and smaller values of 'cex'
            ## to get the string small enough for the node.  If we hit the
            ## minimum font size, don't output the label and signal a
            ## warning.

            ## This won't be uniform though - not sure exactly how
            ## Graphviz takes care of this, they get a uniform scaling,
            ## whereas this will have different font sizes for different
            ## nodes based on the length of the string


            strW <- width+1
            width <- width * .8
            count <- 0
            while (strW > width) {
                x <- strwidth(txt, "inches", cex)
                if (x == strW) {
                    count <- count + 1
                    if (count == 5) {
                        warning("Label ", txt,
                                " is too large for node")
                        return()
                    }
                }
                strW <- x
                if (strW > width)
                    cex <- cex * .9
            }
        }

        text(xLoc, yLoc, txt,
             col=labelColor(txtLabel), cex=cex)
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
    invisible(symbols(nodeX, nodeY, circles=rad, inches=max(rad),
                      fg=fg, bg=bg,add=TRUE))
}

getGraphSize <- function(graph) {
    sizeStr <- getGraphAttr(graph, "size")
    splitSize <- strsplit(sizeStr, ",")[[1]]
    return(as.numeric(splitSize))
}
