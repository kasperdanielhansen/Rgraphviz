setMethod("plot", "graph",
  function(x, y, ...,
           subGList=list(),
           attrs=list(),
           nodeAttrs=list(),
           edgeAttrs=list(),
           xlab="", ylab="",
           main=NULL, sub=NULL,
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

    invisible(plot(g,
                   xlab=xlab, ylab=ylab,
                   main=main, sub=sub))
  })


setMethod("plot", "Ragraph",
  function(x, y, ...,
           xlab="", ylab="",
           main=NULL, sub=NULL,
           drawNode=drawAgNode) {
           ## nodeAttrs=list(),
           ## edgeAttrs=list()) {

    plot.new()

    ## Some plots can get clipped and shouldn't be.
    ## Change the clip setting to clip to the figure
    ## region (should it be the device region?  I
    ## don't think so, but perhaps).  As far as I
    ## can tell, this doesn't cause any problems
    ## with bounding box issues and the like.
    op <- par(no.readonly=TRUE)
    par(xpd=TRUE)
    on.exit(par(op), add=TRUE)
    
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
    
    ## !! Currently hardcoding log & asp,
    ## !! probably want to change that over time.
    plot.window(xlim=c(0,getX(ur)),
                ylim=c(0,getY(ur)),
                log="", asp=NA, ...)
    xy <- xy.coords(NA, NA, xlab, ylab, "")
    ## !! Also still hardcoding 'type'
    plot.xy(xy, type="n", ...)
    
    title(main=main, xlab=xlab, ylab=ylab, sub=sub)

    ## -----------------------------------------------------------------------
    ## determine whether node labels fit into nodes and set "cex" accordingly
    ## -----------------------------------------------------------------------
    agn <- AgNode(x)
    nodeWidths <- sapply(agn, function(n) { getNodeRW(n)+getNodeLW(n) })
    strWidths  <- sapply(agn, function(n) {
      rv <- strwidth(labelText(txtLabel(n)))
      if(length(rv)==0)
        rv <- 0
      return(rv)} )
    cex <- min(nodeWidths / strWidths) * .9
    if(is.finite(cex))
      par(cex=cex)
  
    ## -----------
    ## draw
    ## -----------
    if (length(drawNode) == 1) {
      lapply(agn, drawNode)
    } else {
      if (length(drawNode) == length(AgNode(x))) {
        for (i in seq(along=drawNode)) {
          drawNode[[i]](agn[[i]])
        }
      } else {
        stop(paste("Length of the drawNode parameter is ", length(drawNode), 
                   ", it must be either length 1 or the number of nodes.", sep=""))
      } ## else
    } ## else
    
    ## Use the smallest node radius as a means to scale the size of 
    ## the arrowheads -- in INCHES! see man page for "arrows", which is called
    ## from bLines, which is called from lines.
    arrowLen <- par("pin")[1] / diff(par("usr")[1:2]) * min(nodeWidths) / (2*pi)
    ## Plot the edges
    lapply(AgEdge(x), lines, len=arrowLen, edgemode=edgemode)
    
    invisible(x)
  })


drawAgNode <- function(node) {
  nodeCenter <- getNodeCenter(node)
  nodeX <- getX(nodeCenter)
  nodeY <- getY(nodeCenter)

  lw     <- getNodeLW(node)
  rw     <- getNodeRW(node)
  rad    <- (lw+rw)/2
  height <- getNodeHeight(node)
  fg     <- color(node)
  bg     <- fillcolor(node)
  
  switch(shape(node),
         "circle"    = Rgraphviz:::drawCircleNode(x=nodeX, y=nodeY,
                                      rad=rad, fg=fg, bg=bg),
         
         "ellipse"   = Rgraphviz:::ellipse(x=nodeX, y=nodeY,
                    height=height, width=rad*2, fg=fg, bg=bg),
         "box"=,
         "rect"=,
         "rectangle" = rect(nodeX-lw, nodeY-(height/2), nodeX+rw,
           nodeY+(height/2), col=bg, border=fg),
         
         "plaintext"= { if (style(node) == "filled")
                          rect(nodeX-lw, nodeY-(height/2),
                               nodeX+rw, nodeY+(height/2),
                               col=bg, border=FALSE) },
         stop("Unimplemented node shape: ", shape)
         ) ## switch
  
  drawTxtLabel(txtLabel(node), xLoc=nodeX, yLoc=nodeY)
}

drawTxtLabel <- function(txtLabel, xLoc, yLoc) {
  txt <- labelText(txtLabel)
  if(length(txt)>1)
    stop("'labelText(txtLabel)' must have length 1.")
  if(length(txt)==0)
    return(invisible(NULL))

  if(xor(missing(xLoc), missing(yLoc)))
    stop("'xLoc' and 'yLoc' must be either be both specified or both missing.")
  if(missing(xLoc)) {  
    loc <- labelLoc(txtLabel)
    justMod <- switch(labelJust(txtLabel),
                      "l" = 0,
                      "n" = -0.5,
                      "r" = -1)
    xLoc <-  getX(loc) + (justMod * labelWidth(txtLabel))
    yLoc <-  getY(loc)
  }
  
  text(xLoc, yLoc, txt, col=labelColor(txtLabel))
}

## Not sure what this is good for:
## getRadiusDiv <- function(ur) {
##   return(max(c(getX(ur), getY(ur))/par("pin")))
## }

drawCircleNode <- function(x, y, rad, fg, bg) {
    invisible(symbols(x, y, circles=rad, inches=max(rad),
                      fg=fg, bg=bg, add=TRUE))
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

