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

                  g <- agopen(x, "ABC", layout=TRUE, layoutType=y,
                              attrs=attrs, nodeAttrs=nodeAttrs,
                              edgeAttrs=edgeAttrs, subGList=subGList,
                              recipEdges=recipEdges)

                  invisible(plot(g, xlab=xlab,
                                 ylab=ylab, main=main, sub=sub))
              })


    setMethod("plot", "Ragraph",
              function(x, y, ..., xlab="", ylab="", main=NULL, sub=NULL,
                       drawNode=drawAgNode, nodeAttrs=list(),
                       edgeAttrs=list()) {
                  plot.new()

                  ## !!!!
                  ## FIXME: Do we still need node/edge/subGAttrs here?
                  ## !!!!


                  ## Some plots can get clipped and shouldn't be.
                  ## Change the clip setting to clip to the figure
                  ## region (should it be the device region?  I
                  ## don't think so, but perhaps).  As far as I
                  ## can tell, this doesn't cause any problems
                  ## with bounding box issues and the like.
                  op <- par(xpd=TRUE)
                  on.exit(par(op), add=TRUE)

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

                      radConv <- getRadiusDiv(ur)

                      if (length(drawNode) == 1)
                         lapply(AgNode(x), drawNode, ur, nodeAttrs, radConv)
                      else if (length(drawNode) == nNodes) {
                          nodes <- AgNode(x)
                          for (i in 1:nNodes) {
                              curDrawFun <- drawNode[[i]]
                              curDrawFun(nodes[[i]], ur, nodeAttrs, radConv)
                          }
                      }
                      else
                          stop("Length of the drawNode parameter",
                               " must be either length 1 or the",
                               " number of nodes.")

                      ## Use the smallest node radius as a means
                      ## to scale the size of the arrowheads in
                      ## directed graphs
                      arrowLen <- min(sapply(AgNode(x), getNodeRW)) / (radConv*3)
                      ## Plot the edges
                      q <- lapply(AgEdge(x), function(x, arrowLen,
                                                      edgemode, ur,
                                                      attrs) {
                          ## See if there's a specified edgeCol for this
                          if (!is(x,"AgEdge"))
                              stop(paste("Class:",class("AgEdge")))
                          lines(x, len=arrowLen, edgemode=edgemode,
                                attrs=attrs)
                      }, arrowLen, edgemode(x), ur, edgeAttrs)
                  }
                  else {
                      stop("No nodes in graph")
                  }

                  invisible(x)
              })
}


drawAgNode <- function(node, ur, attrs=list(), conv=1) {
    nodeName <- name(node)

    ## First get X/Y
    nodeCenter <- getNodeCenter(node)
    nodeX <- getX(nodeCenter)
    nodeY <- getY(nodeCenter)

    rw <- getNodeRW(node)
    lw <- getNodeLW(node)
    height <- getNodeHeight(node)

    rad <- rw / conv

    attrNames <- names(attrs)
    if (("color" %in% attrNames)&&(nodeName %in% names(attrs$color)))
        fg <- as.character(attrs$color[nodeName])
    else
        fg <- color(node)

    if (("fillcolor" %in% attrNames)&&(nodeName %in% names(attrs$fillcolor)))
        bg <- as.character(attrs$fillcolor[nodeName])
    else
        bg <- fillcolor(node)

    if (("shape" %in% attrNames)&&(nodeName %in% names(attrs$shape)))
        shape <- as.character(attrs$shape[nodeName])
    else
        shape <- shape(node)


    if (("style" %in% attrNames)&&(nodeName %in% names(attrs$style)))
        style <- as.character(attrs$style[nodeName])
    else
        style <- style(node)


    switch(shape,
           "circle"=drawCircleNode(nodeX, nodeY, ur, rad, fg, bg),
           "ellipse"=ellipse(nodeX, nodeY, height=height, width=rw*2,
                             fg=fg, bg=bg),
           "box"=,
           "rect"=,
           "rectangle"=rect(nodeX-lw, nodeY-(height/2), nodeX+rw,
                            nodeY+(height/2), col=bg, border=fg),
           "plaintext"= {if (style == "filled")
                             rect(nodeX-lw, nodeY-(height/2),
                                  nodeX+rw, nodeY+(height/2),
                                  col=bg, border=FALSE)},
           stop("Unimplemented node shape: ", shape))

    drawTxtLabel(txtLabel(node), nodeX, nodeY, width=rad*2, nodeName, attrs)
}

drawTxtLabel <- function(txtLabel, xLoc, yLoc, width, objName,
                         objAttrs=list()) {
    if ((!is.null(txtLabel))&&(length(labelText(txtLabel)) > 0)) {
        attrNames <- names(objAttrs)

        if (("label" %in% attrNames)&&(objName %in% names(objAttrs$label)))
            txt <- as.character(objAttrs$label[objName])
        else
            txt <- labelText(txtLabel)

        loc <- labelLoc(txtLabel)

        if (("labeljust" %in% attrNames)&&(objName %in% names(objAttrs$labeljust)))
            labelJust <- as.character(objAttrs$labeljust[objName])
        else
            labelJust <- labelJust(txtLabel)

        if (missing(xLoc)) {
            justMod <- switch(labelJust,
                              "l" = 0,
                              "n" = -0.5,
                              "r" = -1)

            xLoc <- getX(loc) + (justMod * labelWidth(txtLabel))
        }
        if (missing(yLoc))
            yLoc <- getY(loc)

        ## Get the font size that Graphviz believes this to be
        op <- par()

        if (("labelfontsize" %in% attrNames)&&
            (objName %in% names(objAttrs$labelfontsize)))
            curFontsize <- as.numeric(objAttrs$labelfontsize[objName])
        else
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

        if (("labelfontcolor" %in% attrNames)&&
            (objName %in% names(objAttrs$labelfontcolor)))
            labelColor <- objAttrs$labelfontcolor[objName]
        else
            labelColor <- labelColor(txtLabel)

        text(xLoc, yLoc, txt, col=labelColor, cex=cex)
    }
}

getRadiusDiv <- function(ur) {
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

    conv
}

drawCircleNode <- function(nodeX, nodeY, ur, rad, fg, bg) {
    invisible(symbols(nodeX, nodeY, circles=rad, inches=max(rad),
                      fg=fg, bg=bg,add=TRUE))
}

identifyGraph <- function(plotGraph, ...) {

    ## Get the information for the nodes (x, y, labels)
    xy <- getNodeXY(plotGraph)
    nodes <- sapply(AgNode(plotGraph), name)

    ## Now get the edges
    edges <- AgEdge(plotGraph)
    edgeNames <- sapply(edges, function(x) paste(tail(x),
                                                 head(x), sep="~"))

    edgeLabels <- character()
    vals <- matrix(nrow = 0, ncol = 2)

    for (i in 1:length(edges)) {
        for (j in 1:numSplines(edges[[i]])) {
            cur <- bezierPoints(getSpline(edges[[i]], j))
            edgeLabels <- c(edgeLabels, rep(edgeNames[i], nrow(cur)))
            vals <- rbind(vals, cur)
        }
    }

    labels <- c(nodes, edgeLabels)
    sel <- identify(c(xy$x, vals[,1]), c(xy$y, vals[,2]), labels, ...)

    list(points=sel, labels=labels)
}

