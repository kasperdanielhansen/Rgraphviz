## Wrapper around paste for simple yet flexible namespacing of the attribute
## names
myAtt <- function(att)
  paste(prefix="rgraphviz:::", att, sep="")



## This function will draw individual nodes to the plotting device.
## The input is a list of nodeData as produced by graph's nodeData
## function. Usually it will be called via lapply as in the following
## example:
## lapply(nodeData(thegraph), drawAgNode)
## For now this has to be called for each node separately because the user
## can give different drawing functions for each node (do we really need that?)
## FIXME: In the end this should all be vectorized if possible
myDrawAgNode <- function(node) { 
  nodeX <- node[[myAtt("x")]]
  nodeY <- node[[myAtt("y")]]
  lw <- node[[myAtt("lWidth")]]
  rw <- node[[myAtt("rWidth")]]
  rad    <- (lw+rw)/2
  height <- node[[myAtt("height")]]
  fg <- node[[myAtt("color")]]
  style <- node[[myAtt("style")]]
  shape <- node[[myAtt("shape")]]

  ## Make sure the colors and shapes are set OK
  if (shape =="") shape <- "ellipse" 
  if (fg == "") fg <- "black"
  bg <- node[[myAtt("fillcolor")]]
  if (bg == "") {
      if (style == "filled") bg <- "grey"
      else bg <- "transparent"
  }

 ## Normal Rgraphviz defaults to circle, but DOT defaults to ellipse
  switch(shape,
         "circle"    = Rgraphviz:::drawCircleNode(x=nodeX, y=nodeY,
                                      rad=rad, fg=fg, bg=bg),

         "ellipse"   = Rgraphviz:::ellipse(x=nodeX, y=nodeY,
                    		   height=height, width=rad*2, fg=fg, bg=bg),
         "box"=,
         "rect"=,
         "rectangle" = rect(nodeX-lw, nodeY-(height/2), 
			    nodeX+rw, nodeY+(height/2), 
			    col=bg, border=fg),

         "plaintext"= { if (style == "filled")
                          rect(nodeX-lw, nodeY-(height/2),
                               nodeX+rw, nodeY+(height/2),
                               col=bg, border=FALSE) },
         stop("Unimplemented node shape: ", shape(node))
         ) ## switch


  myDrawTxtLabel(node, xLoc=nodeX, yLoc=nodeY)
}


## This function draws the node and edge labels on the plotting device.
## It is called by myDrawAgNode. The input is again a list of nodeData
## as well as the x and y location of the nodes.
## FIXME: This doesn't have to be called every time by myDrawAgNode. It could be
## a vectorized version of text that gets called after the node plotting.
myDrawTxtLabel <- function(attr, xLoc, yLoc) {
  txt <- attr[[myAtt("label")]]

  if(length(txt)>1) stop("label must have length 1.")

  if(length(txt)==0 || txt=="") return(invisible(NULL))

  if(xor(missing(xLoc), missing(yLoc)))
    stop("'xLoc' and 'yLoc' must be either be both specified or both missing.")

  ## When there's no x location it is computed according to the adjustment
  if(missing(xLoc)) {
    lj <- attr[[myAtt("labelJust")]]
    if(length(lj)){
      justMod <- switch(lj,
                        "l" = 0,
                        "n" = -0.5,
                        "r" = -1)
      xLoc <-   as.numeric(attr[[myAtt("labelX")]]) +(justMod * as.numeric(attr[[myAtt("labelWidth")]]))
      yLoc <-  as.numeric(attr[[myAtt("labelY")]])
    }else{
      xLoc <- yLoc <- 0
    }
  }
  
  ## NOTE: labelFontsize is translated into cex parameter: fontsize 14 = cex 1 
  text(xLoc, yLoc, txt, col=attr[[myAtt("fontcolor")]],
       cex=as.numeric(attr[[myAtt("fontsize")]])/14)
}


## Draw the edges from the Bezier curves.
myEdgeLines <- function(edge, ..., len, edgemode){
  z <- edge[[myAtt("splines")]]
  edgeColor <- edge[[myAtt("color")]]
  if (edgeColor == "") edgeColor <- "black"
  arrowSize <- edge[[myAtt("arrowsize")]]
  if ( arrowSize == "" ) arrowSize = "1"
  len <- len * as.numeric(arrowSize)
  lty <- edge[[myAtt("lty")]]
  lwd <- edge[[myAtt("lwd")]]
  
  mapply(lines, z, MoreArgs=list(len=len, col=edgeColor, lty=lty, lwd=lwd, ...))

  dir <- edge[[myAtt("dir")]]
  ## TODO: arrow shapes should/could be from arrowtail/head 
  if(dir == "both" || dir == "back")
    {
      tails = bezierPoints(z[[1]])
      tail_from = tails[2, ]
      tail_to   = tails[1, ]
      arrows(tail_from[1], tail_from[2], tail_to[1], tail_to[2],
			col=edgeColor, length=len, lty=lty, lwd=lwd)
    }
    if(dir == "both" || dir == "forward")
    {
       heads = bezierPoints(z[[length(z)]])
       head_from = heads[nrow(heads)-1, ]
       head_to   = heads[nrow(heads),]
       arrows(head_from[1], head_from[2], head_to[1], head_to[2],
			col=edgeColor, length=len, lty=lty, lwd=lwd)
    }

    myDrawTxtLabel(edge)
}
  

## Grab the node and node label information from an Ragraph
## and put it into graph's nodeData
## This will extract:
##   - rWidth: the right half of the node in points
##   - lWidth: the left half of the node in points
##   - height: the height of the node in points
##   - x: the x location of the node
##   - y: the y location of the node
##   - labelX: the x location of the node label
##   - labelY: the y location of the node label
##   - labelJust: the adjustment of the node label
##   - labelWidth: the width of the node label
##   - style: no idea where this comes from
## FIXME: Need vectorization here, this is terribly inefficient
nodeRagraph2graph <- function(g, x){
  ## get everything from the Ragraph
  agn <- AgNode(g)
  nnames <- sapply(agn, slot, "name")
  rw <- sapply(agn, getNodeRW)
  lw <- sapply(agn, getNodeLW)
  height <- sapply(agn, getNodeHeight)
  centerX <- sapply(agn, function(f) getNodeCenter(f)@x)
  centerY <- sapply(agn, function(f) getNodeCenter(f)@y)
  labelX <- sapply(agn, function(f) labelLoc(txtLabel(f))@x)
  labelY <- sapply(agn, function(f) labelLoc(txtLabel(f))@y)
  labelJust <- sapply(agn, function(f) labelJust(txtLabel(f)))
  labelWidth <- sapply(agn, function(f) labelWidth(txtLabel(f)))
  style <- sapply(agn, style)
  
  ## fill nodeData
  for(n in seq(along=nnames)){
    nodeData(x, nnames[n], myAtt("rWidth")) <- rw[n] 
    nodeData(x, nnames[n], myAtt("lWidth")) <- lw[n]
    nodeData(x, nnames[n], myAtt("height")) <- height[n]
    nodeData(x, nnames[n], myAtt("x")) <- centerX[n]
    nodeData(x, nnames[n], myAtt("y")) <- centerY[n]
    nodeData(x, nnames[n], myAtt("labelX")) <- labelX[n]
    nodeData(x, nnames[n], myAtt("labelY")) <- labelY[n]
    nodeData(x, nnames[n], myAtt("labelJust")) <- labelJust[n]
    nodeData(x, nnames[n], myAtt("labelWidth")) <- labelWidth[n]
    nodeData(x, nnames[n], myAtt("style")) <- style[n]
  }
  return(x)
}


## Grab the edge and edge label information from an Ragraph
## and put it into graph's edgeData
## This will extract:
##   - splines: the splines of the Bezier curve
##   - labelX: the x location of the edge label
##   - labelY: the y location of the node label
##   - labelJust: the adjustment of the edge label
##   - labelWidth: the width of the edge label
##   - arrowhead: the type of arrow heads for directed graphs
##   - arrowtail: the type of arrow tails for directed graphs
##   - dir: the direction of arrows for directed graphs
## FIXME: Need vectorization here, this is terribly inefficient
edgeRagraph2graph <- function(g, x){
  ## get everything from the Ragraph
  age <- AgEdge(g)
  enamesFrom <- sapply(age, slot, "tail")
  enamesTo <- sapply(age, slot, "head")
  splines <- lapply(age, splines)
  labelX <- sapply(age, function(f) labelLoc(txtLabel(f))@x)
  labelY <- sapply(age, function(f) labelLoc(txtLabel(f))@y)
  labelJust <- sapply(age, function(f) labelJust(txtLabel(f)))
  labelWidth <- sapply(age, function(f) labelWidth(txtLabel(f)))
  arrowhead <- sapply(age, arrowhead)
  arrowtail <- sapply(age, arrowtail)
  dir <- sapply(age, slot, "dir")

  ## fill edgeData
  for(n in seq(along=enamesFrom)){
    edgeData(x, enamesFrom[n], enamesTo[n], myAtt("splines")) <- splines[n]
    edgeData(x, enamesFrom[n], enamesTo[n], myAtt("labelX")) <- labelX[n]
    edgeData(x, enamesFrom[n], enamesTo[n], myAtt("labelY")) <- labelY[n]
    edgeData(x, enamesFrom[n], enamesTo[n], myAtt("labelJust")) <- labelJust[n]
    edgeData(x, enamesFrom[n], enamesTo[n], myAtt("labelWidth")) <- labelWidth[n]
    edgeData(x, enamesFrom[n], enamesTo[n], myAtt("arrowhead")) <- arrowhead[n]
    edgeData(x, enamesFrom[n], enamesTo[n], myAtt("arrowtail")) <- arrowtail[n]
    edgeData(x, enamesFrom[n], enamesTo[n], myAtt("dir")) <- dir[n]
  }
  return(x)
}

## Grab the graph-wide information from an Ragraph
## and put it into graph's graphData
## This will extract:
##   - bbox: the bounding box of the graph
graphRagraph2graph <- function(g, x){
  ## get everything from the Ragraph
  bbox <- matrix(c(g@boundBox@botLeft@x, g@boundBox@botLeft@y,
                   g@boundBox@upRight@x, g@boundBox@upRight@y),
                  ncol=2, byrow=T)

  ## fill graphData
  x@graphData[[myAtt("bbox")]] <- bbox
  x@graphData[[myAtt("laydout")]] <- TRUE #set laydout flag
  return(x)
}




#############################################################################
##---------------------------------------------------------------------------
#############################################################################

## This method will call agopen only to receive the necessary layout
## information of the graph which will be stored in the nodeData and
## edgeData slots. The following plotting will be done using method render
## and is completely independent of the Ragraph.
## FIXME: Need better generic for this, layout is already used,
##        subGraphs need to be incoorporated

setGeneric("layoutg", function(x, layout="dot", name="", recipEdges=c("combined", "distinct")) standardGeneric("layoutg"))
           
setMethod("layoutg", "graph",
  function(x, layout="dot", name="", recipEdges=c("combined", "distinct")){

    if (!validGraph(x))
      stop("The graph to be plotted is not a valid graph structure")
    recipEdges <- match.arg(recipEdges)

    ## the defaults for graph, custer, edge and node attributes
    tmpAtt <- getDefaultAttrs(list(), layout)
    defNodeAttrs <- tmpAtt$node
    defEdgeAttrs <- tmpAtt$edge
    defGraphAttrs <- tmpAtt$graph
    defClustAttrs <- tmpAtt$cluster #FIXME: What to do with those?

    ## Set nodeData in the graph instance if missing.
    ## The default for label text will be set to node names
    prefix <- "rgraphviz:::"
    nn <- names(x@nodeData@defaults)
    nd <- myAtt(names(defNodeAttrs))
    missNodeDefs <- nd[!nd %in% nn]
    for(n in missNodeDefs)
      nodeDataDefaults(x, attr=n) <- defNodeAttrs[[gsub(prefix, "", n)]]
    for(n in nodes(x)){
      ndl <- nodeData(x, n, attr=myAtt("label"))
      if(ndl == "\\N")
        nodeData(x, n, attr=myAtt("label")) <- n
    }

    ## Set necessary additional defaults in graph's nodeData
    na <- c("rWidth", "lWidth", "x", "y", "labelX", "labelY",
            "labelJust", "labelWidth", "style")
    defs <- list(1,1,0,0,0,0,"n",0,"")
    nn <- gsub(prefix, "", names(x@nodeData@defaults))
    for(i in which(!na %in% nn))
      nodeDataDefaults(x, myAtt(na[i])) <- defs[[i]]
   
    ## set edgeData defaults in the graph instance if missing.
    en <- names(x@edgeData@defaults)
    ed <- myAtt(names(defEdgeAttrs))
    missEdgeDefs <- ed[!ed %in% en]
    for(n in missEdgeDefs)
      edgeDataDefaults(x, attr=n) <- defEdgeAttrs[[gsub(prefix, "", n)]]

    ## Set necessary additional defaults in graph's edgeData
    ea <- c("splines", "labelX", "labelY",
            "labelJust", "labelWidth")
    defs <- list(list(),0,0,"n",0)
    en <- gsub(prefix, "", names(x@edgeData@defaults))
    for(i in which(!ea %in% en))
      edgeDataDefaults(x, myAtt(ea[i])) <- defs[[i]]
    
    ## set graphData in the graph instance if missing.
    ## FIXME: There are no methods for this in graph yet.
    defGraphAttrs$laydout <- FALSE #flag to indicate layout status
    gn <- names(x@graphData)
    gd <- myAtt(names(defGraphAttrs))
    missGraphDefs <- gd[!gd %in% gn]
    ga <- defGraphAttrs[gsub(prefix, "", missGraphDefs)]
    names(ga) <- missGraphDefs
    for(n in names(ga))
      x@graphData[[n]] <- ga[[n]]
   
    
    ## ##########################################################
    ## layout graph:
    ## ##########################################################
    ## call agopen for now only to get the edge and node coordinates
    ## and the label locations and save all layout information to
    ## the graph
    nattrs <- list(label=unlist(nodeData(x, att=myAtt("label"))),
                   fixedsize=unlist(nodeData(x, att=myAtt("fixedsize"))))
    eattrs <- list(label=unlist(edgeData(x, att=myAtt("label"))),
                   arrowhead=unlist(edgeData(x, att=myAtt("arrowhead"))),
                   arrowtail=unlist(edgeData(x, att=myAtt("arrowtail"))),
                   arrowsize=unlist(edgeData(x, att=myAtt("arrowsize"))))
    
    g <- agopen(x, name="test", layoutType=layout, nodeAttrs=nattrs,
                recipEdges=recipEdges)
    x <- nodeRagraph2graph(g,x)
    x <- edgeRagraph2graph(g,x)
    x <- graphRagraph2graph(g,x)

    ## The edgeDataDefault for lwd and lty will be set to default
    ## par settings if missing at this point
    if(!myAtt("lwd") %in% names(x@edgeData@defaults))
      edgeDataDefaults(x, attr=myAtt("lwd")) <- 1
    if(!myAtt("lty") %in% names(x@edgeData@defaults))
      edgeDataDefaults(x, attr=myAtt("lty")) <- "solid"
    
    return(x)
  })
          


############################################################
## render graph to plotting device
############################################################
setGeneric("renderg", function(x, ..., main=NULL, cex.main=NULL, col.main="black",
           sub=NULL, cex.sub=NULL, col.sub="black",
           drawNode=myDrawAgNode, xlab, ylab) standardGeneric("renderg"))

setMethod("renderg", "graph",  
  function(x, ..., main=NULL, cex.main=NULL, col.main="black",
           sub=NULL, cex.sub=NULL, col.sub="black",
           drawNode=myDrawAgNode, xlab, ylab){
    
  
    if(is.null(x@graphData[[myAtt("laydout")]]) || !x@graphData[[myAtt("laydout")]])
      stop("Graph has not been layd out yet. Please use method'layoutg'")
    plot.new()

    ## eliminate all plot borders
    old.mai=par(mai=0.01+c(0.83*(!is.null(sub)), 0, 0.83*(!is.null(main)), 0))
    on.exit(par(mai=old.mai), add=TRUE)

    ## This grabs the bg and fg color from the default graph attributes.
    ## FIXME: Should this be stored in the graph as well? Where?
    ##        Where does the fg come from? It's not in the defaults. For now
    ## I hardcode this to "black"
    bg <- x@graphData[[myAtt("bgcolor")]]
    par(bg=bg)
    par(fg="black")

    ## Set up the plot region.  We need
    ## to emulate what happens in 'plot.default' as
    ## we called plot.new() above, and for the same
    ## reasons as doing that, calling 'plot' now
    ## will mung up the thing if people are using
    ## 'layout' with this.

    ## !! Currently hardcoding log & asp,
    ## !! probably want to change that over time.
    bbox <- x@graphData[[myAtt("bbox")]]
    plot.window(xlim=c(bbox[,1]), ylim=c(bbox[,2]), log="", asp=NA, ...)
    xy <- xy.coords(NA, NA)

    ## !! Also still hardcoding 'type'
    plot.xy(xy, type="n", ...)

    ## That's really strange! Should simply be ignored...
    if(!missing(xlab) && !missing(ylab))
      stop("Arguments 'xlab' and 'ylab' are not handled.")

    ## Add title if necessary
    if(!is.null(sub)||!is.null(main))
      title(main, sub, cex.main=cex.main, col.main=col.main, 
		       cex.sub=cex.sub, col.sub=col.sub)
    
    ## determine whether node labels fit into nodes and set "cex" accordingly
    nodeDims <- t(cbind((unlist(nodeData(x, attr=myAtt("rWidth"))) +
                  unlist(nodeData(x, attr=myAtt("lWidth")))),
                  unlist(nodeData(x, attr=myAtt("height")))))
    strDims  <- sapply(nodeData(x, attr=myAtt("label")),
                       function(s) {
      			if(length(s)==0) {
        			rv <- c(strwidth(" "), strheight(" "))
      			} else {
        			rv <- c(strwidth(s)*1.1, strheight(s)*1.4)
      			}
      			return(rv)
    			} )
    cex <- min(nodeDims / strDims)
    if(is.finite(cex)) {
      old.cex <- par(cex=cex)
      on.exit(par(cex=old.cex), add=TRUE)
    }

    ## draw nodes
    if (length(drawNode) == 1) {
      lapply(nodeData(x), drawNode)
    } else {
      ## individual drawing function for each node
      if (length(drawNode) == length(AgNode(x))) {
        for (i in seq(along=drawNode)) {
          drawNode[[i]](nodeData(x)[[i]])
        }
      } else {
        stop(paste("Length of the drawNode parameter is ", length(drawNode),
                   ", it must be either length 1 or the number of nodes.", sep=""))
      } ## else
    } ## else

    ## Use the smallest node radius as a means to scale the size of
    ## the arrowheads -- in INCHES! see man page for "arrows", which is called
    ## from bLines, which is called from lines.
    arrowLen <- par("pin")[1] / diff(par("usr")[1:2]) * min(nodeDims) / pi

    ## Plot the edges
    lapply(edgeData(x), myEdgeLines, len=arrowLen, edgemode=edgemode(x))

    return(invisible(x))

  })


