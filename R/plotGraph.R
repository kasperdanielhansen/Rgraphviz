setMethod("plot", "graph",
  function(x, y, ..., name="",
           subGList=list(),
           attrs=list(),
           nodeAttrs=list(),
           edgeAttrs=list(),
           recipEdges=c("combined", "distinct")){
    if (!validGraph(x))
      stop("The graph to be plotted is not a valid graph structure")

    if (missing(y)) y <- "dot"

    recipEdges <- match.arg(recipEdges)

    g <- agopen(x, name=name, layout=TRUE, layoutType=y,
                attrs=attrs, nodeAttrs=nodeAttrs,
                edgeAttrs=edgeAttrs, subGList=subGList,
                recipEdges=recipEdges)

    invisible(plot(g, y, ...))
})

setMethod("plot", "Ragraph",
  function(x, y, edgeAttrs=list(), ...,
           main=NULL, cex.main=NULL, col.main="black",
           sub=NULL, cex.sub=NULL, col.sub="black",
           drawNode=drawAgNode, xlab, ylab) {
      ## layout graph
      if ( missing(y) ) y <- x@layoutType
      x <- graphLayout(x, y)

      ## render graph
      plot.new()
      opar <- par(no.readonly = TRUE)
      on.exit(par(opar))
      
      ## eliminate all plot borders
      par(mai=0.01+c(0.83*(!is.null(sub)), 0, 0.83*(!is.null(main)), 0))
      
      ## Get the upper right/bottom left points of the bounding box for the graph
      ur <- upRight(boundBox(x))
      bl <- botLeft(boundBox(x))
      
      if ( x@bg != "" ) par(bg=x@bg)
      if ( x@fg != "" ) par(fg=x@fg)
      
      ## Set up the plot region.  We need
      ## to emulate what happens in 'plot.default' as
      ## we called plot.new() above, and for the same
      ## reasons as doing that, calling 'plot' now
      ## will mung up the thing if people are using
      ## 'layout' with this.
      
      ## !! Currently hardcoding log & asp,
      ## !! probably want to change that over time.
      plot.window(xlim=c(getX(bl),getX(ur)),
                  ylim=c(getY(bl),getY(ur)),
                  log="", asp=NA, ...)
      xy <- xy.coords(NA, NA)
      
      ## !! Also still hardcoding 'type'
      plot.xy(xy, type="n", ...)
      
      if(!missing(xlab) && !missing(ylab))
          stop("Arguments 'xlab' and 'ylab' are not handled.")
      
      if(!is.null(sub)||!is.null(main))
          title(main, sub, cex.main=cex.main, col.main=col.main, 
                cex.sub=cex.sub, col.sub=col.sub)
      
      ## -----------------------------------------------------------------------
      ## determine whether node labels fit into nodes and set "cex" accordingly
      ## -----------------------------------------------------------------------
      agn <- AgNode(x)
      nodeDims <- sapply(agn, function(n) 
                     { c(getNodeRW(n)+getNodeLW(n), getNodeHeight(n)) })
      strDims  <- sapply(agn, function(n) {
          s  <- labelText(txtLabel(n))
          if(length(s)==0) {
              rv <- c(strwidth(" "), strheight(" "))
          } else {
        			rv <- c(strwidth(s)*1.1, strheight(s)*1.4)
                            }
          return(rv)
    			} )
      cex <- min(nodeDims / strDims)
      if(is.finite(cex) && cex > 0 ) {
          par(cex=cex)
      }
      
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
      ## the arrowheads -- in INCHES! see man page for "arrows", 
      arrowLen <- par("pin")[1] / diff(par("usr")[1:2]) * min(nodeDims) / pi
      
      ## Plot the edges
      lapply(AgEdge(x), lines, len=arrowLen, edgemode=edgemode, ...)
      
      invisible(x)
})

drawAgNode <- function(node) {
  nodeCenter <- getNodeCenter(node)
  nodeX <- getX(nodeCenter)
  nodeY <- getY(nodeCenter)

  lw <- getNodeLW(node)
  rw <- getNodeRW(node)
  rad <- (lw+rw)/2
  height <- getNodeHeight(node)
  fg <- color(node)
  style <- style(node)
  shape <- shape(node)
  border.lwd <- border.lwd(node)
  border.color <- border.color(node)

  if (border.lwd == "")
      border.lwd <- 1
  if (border.color == "")
      border.color <- "black"

  ## Normal Rgraphviz defaults to circle, but DOT defaults to ellipse
  if (shape =="") shape <- "ellipse" 

  if (fg == "") fg <- "black"
  bg <- fillcolor(node)
  if (bg == "") {
      if (style == "filled") bg <- "grey"
      else bg <- "transparent"
  }

  switch(shape,
         "circle"    = symbols(nodeX, nodeY, circles=rad, inches=FALSE, fg=fg, bg=bg, add=TRUE),
         "ellipse"   = ellipse(x=nodeX, y=nodeY, height=height, width=rad*2, fg=fg, bg=bg),
         "box"=,
         "rect"=,
         "rectangle" = rect(nodeX-lw, nodeY-(height/2), 
			    nodeX+rw, nodeY+(height/2), 
			    col=bg, border=border.color, lwd = border.lwd),
         "plaintext"= { if (style == "filled")
                          rect(nodeX-lw, nodeY-(height/2),
                               nodeX+rw, nodeY+(height/2),
                               col=bg, border=FALSE) },
         stop("Unimplemented node shape: ", shape(node))
         ) ## switch

  drawTxtLabel(txtLabel(node), xLoc=nodeX, yLoc=nodeY)
}

drawTxtLabel <- function(txtLabel, xLoc, yLoc) {
  ## NOTE: labelFontsize not used

  txt <- labelText(txtLabel)

  if(length(txt)>1) stop("'labelText(txtLabel)' must have length 1.")

  if(length(txt)==0) return(invisible(NULL))

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

