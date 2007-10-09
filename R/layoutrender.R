## Wrapper around paste for simple yet flexible namespacing of the attribute
## names

## myAtt <- function(att)
##   paste(prefix="rgraphviz_", att, sep="")

myAtt <- function(att) att


## This function will draw individual nodes to the plotting device.
## The input is a list of nodeData as produced by graph's nodeData
## function. Usually it will be called via lapply as in the following
## example:
## lapply(nodeData(thegraph), drawAgNode)
## For now this has to be called for each node separately because the user
## can give different drawing functions for each node (do we really need that?)

## FIXME: In the end this should all be vectorized if possible

## myDrawAgNode <- function(node) { ... }

## Update: removed, to be replaced by vectorized form (user can still
## supply a function, but that has to deal with vectorized data)


getLayoutPar <-
    function(name, node.info, length.out)
{
    ans <- node.info[[myAtt(name), exact = TRUE]]
    if (is.null(ans)) # error if length.out not supplied
        rep(graph.par(myAtt(name))[[1]], length.out = length.out)
    else if (!missing(length.out) && length(ans) < length.out)
    {
        ans2 <- rep(ans, length.out = length.out)
        ans2[] <- NA
        ans2[seq_along(ans)] <- ans
        ans2
    }
    else ans
}


renderNodeInfo <-  ## FIXME: node.info should have names already
    function(node.info, node.names) 
{
    nodeX <- getLayoutPar("nodeX", node.info)
    nodeY <- getLayoutPar("nodeY", node.info)
    n <- length(nodeX)
    lw <- getLayoutPar("lWidth", node.info, n)
    rw <- getLayoutPar("rWidth", node.info, n)
    rad    <- (lw+rw)/2
    height <- getLayoutPar("height", node.info, n)
    fill <- getLayoutPar("fill", node.info, n)
    col <- getLayoutPar("col", node.info, n)
    style <- getLayoutPar("style", node.info, n)
    shape <- getLayoutPar("shape", node.info, n)
    label <- getLayoutPar("label", node.info, n)
    if (is.null(label)) label <- node.names

    possible.shapes <-
        c("circle", "ellipse", "box", "rectangle", "plaintext")
    shape <-
        possible.shapes[pmatch(shape,
                               possible.shapes,
                               duplicates.ok = TRUE)]
    ## shape == circle
    i <- shape == "circle"
    if (any(i))
    {
        symbols(nodeX[i], nodeY[i], circles = rad[i],
                fg = col[i], bg = fill[i],
                inches = FALSE, add = TRUE)
    }
    ## shape == box, rect, etc
    i <- shape %in% c("box", "rectangle")
    if (any(i))
    {
        rect(nodeX[i] - lw[i], nodeY[i] - (height[i] / 2),
             nodeX[i] + rw[i], nodeY[i] + (height[i] / 2),
             col = fill[i], border = col[i])
    }
    ## shape == ellipse
    i <- shape == "ellipse"
    if (any(i))
    {
        npoints <- 51
        tt <- c(seq(-pi, pi, length = npoints), NA)
        xx <-
            rep(nodeX[i], each = npoints + 1) +
                sin(tt) * rep(rad[i], each = npoints + 1)
        yy <-
            rep(nodeY[i], each = npoints + 1) +
                cos(tt) * rep(height[i] / 2, each = npoints + 1)
        polygon(xx, yy, border = col[i], col = fill[i])
    }

    ## shape == plaintext
    ## nothing to do (for style = "filled", use fill = "grey")

    ## draw labels

    ## determine whether node labels fit into nodes and set "cex" accordingly
    nodeDims <-
        rbind(x@nodeInfo[[myAtt("rWidth")]] + x@nodeInfo[[myAtt("lWidth")]],
              x@nodeInfo[[myAtt("height")]])
##     strWidths  <- 1.1 * strwidth(ifelse(nzchar(labels), labels, "W"))     ## FIXME: something weird going on
##     strHeights  <- 1.4 * strheight(ifelse(nzchar(labels), labels, "Tg"))
##     strDims <- rbind(strWidths, strHeights)
##     cex <- min(nodeDims / strDims)
    text(nodeX, nodeY, label, cex = 1)
}


renderSpline <-
    function(spline, head = FALSE, tail = FALSE, len = 1, ...)
{
    lapply(spline, lines, ...)
    if (head)
    {
        xy <- tail(bezierPoints(spline[[length(spline)]]), 2)
        arrows(xy[1], xy[3], xy[2], xy[4], length = len)
    }
    if (tail)
    {
        xy <- head(bezierPoints(spline[[1]]), 2)
        arrows(xy[2], xy[4], xy[1], xy[3], length = len)
    }
}


renderEdgeInfo <-
    function(edge.info, len, edgemode)
{
    n <- length(edge.info$enamesFrom)
    ## col, lty, lwd, etc
    ## arrowsize
    ## direction <- getLayoutPar("direction", edge.info, n) ## UNUSED (isn't this redundant?)
    arrowhead <- getLayoutPar("arrowhead", edge.info, n) != "none"
    arrowtail <- getLayoutPar("arrowtail", edge.info, n) != "none"
    for (i in seq_len(n))
    {
        suppressWarnings(renderSpline(edge.info$splines[[i]],
                                      head = arrowhead[i],
                                      tail = arrowtail[i],
                                      len = len))
    }
    ## FIXME: handle labels (try to share code)
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
## (Update: FIXED)



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
  ## FIXME?: agopen should have shape=ellipse when layouttype=dot, but
  ## seems to give circle.  So, we're going to ignore agn@shape and
  ## use g@layoutType instead
  shape <- if (g@layoutType == "dot") "ellipse" else "circle"
  style <- sapply(agn, style)
  x@nodeInfo <- 
      list(rWidth = rw, 
           lWidth = lw, 
           height = height, 
           nodeX = centerX, 
           nodeY = centerY, 
           labelX = labelX, 
           labelY = labelY,
           labelJust = labelJust, 
           labelWidth = labelWidth,
           shape = shape,
           style = style)
  names(x@nodeInfo) <- myAtt(names(x@nodeInfo))
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

  getLabelPos <- function(f, slot = "x"){
      ans <- slot(labelLoc(txtLabel(f)), slot)
      if (length(ans) == 0) NA_real_
      else ans
  }
  getLabelJust <- function(f){
      ans <- labelJust(txtLabel(f))
      if (length(ans) == 0) NA_character_
      else ans
  }
  getLabelWidth <- function(f){
      ans <- labelWidth(txtLabel(f))
      if (length(ans) == 0) NA_integer_
      else ans
  }

  age <- AgEdge(g)
  enamesFrom <- sapply(age, slot, "tail")
  enamesTo <- sapply(age, slot, "head")
  splines <- lapply(age, splines)
  labelX <- sapply(age, getLabelPos, "x")
  labelY <- sapply(age, getLabelPos, "y")
  labelJust <- sapply(age, getLabelJust)
  labelWidth <- sapply(age, getLabelWidth)
  arrowhead <- sapply(age, arrowhead)
  arrowtail <- sapply(age, arrowtail)
  dir <- sapply(age, slot, "dir")

  x@edgeInfo <-
      list(enamesFrom = enamesFrom,
           enamesTo = enamesTo,
           splines = splines,
           labelX = labelX,
           labelY = labelY,
           labelJust = labelJust,
           labelWidth = labelWidth,
           arrowhead = arrowhead,
           arrowtail = arrowtail,
           direction = dir)
  names(x@edgeInfo) <- myAtt(names(x@edgeInfo))
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
    prefix <- "rgraphviz_"
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
setGeneric("renderg",
           function(x, ..., main=NULL, cex.main=NULL, col.main="black",
                    sub=NULL, cex.sub=NULL, col.sub="black",
                    drawNode = myDrawAgNode,
                    xlab, ylab) standardGeneric("renderg"))


setGeneric("renderg",
           function(x, ...) standardGeneric("renderg"))


setMethod("renderg", "graph",
          function(x, ...,
                   drawNodes = renderNodeInfo,
                   drawEdges = renderEdgeInfo,

                   main=NULL, cex.main=NULL, col.main="black",
                   sub=NULL, cex.sub=NULL, col.sub="black",
                   xlab, ylab,

                   graph.pars = list())
      {

          old.pars <- graph.par(graph.pars)
          on.exit(graph.par(old.pars))
          if(is.null(x@graphData[[myAtt("laydout")]]) || !x@graphData[[myAtt("laydout")]])
              stop("Graph has not been laid out yet. Please use method 'layoutg'")
          plot.new()

          ## eliminate all plot borders
          old.mai <- par(mai=0.01+c(0.83*(!is.null(sub)), 0, 0.83*(!is.null(main)), 0))
          on.exit(par(mai=old.mai), add=TRUE)

          ## This grabs the bg and fg color from the default graph attributes.
          ## FIXME: Should this be stored in the graph as well? Where?
          ##        Where does the fg come from? It's not in the defaults. For now
          ## I hardcode this to "black"
          bg <- x@graphData[[myAtt("bgcolor")]]
          par(bg = bg)
          par(fg = "black")

          ## Set up the plot region.  We need
          ## to emulate what happens in 'plot.default' as
          ## we called plot.new() above, and for the same
          ## reasons as doing that, calling 'plot' now
          ## will mung up the thing if people are using
          ## 'layout' with this.

          ## !! Currently hardcoding log & asp,
          ## !! probably want to change that over time.

          bbox <- x@graphData[[myAtt("bbox")]]
          plot.window(xlim = bbox[,1],
                      ylim = bbox[,2],
                      log="", asp=NA)

          ## xy <- xy.coords(NA, NA)
          ##     ## !! Also still hardcoding 'type'
          ##     plot.xy(xy, type="n", ...)

          ## That's really strange! Should simply be ignored...
          if(!missing(xlab) || !missing(ylab))
              warning("Arguments 'xlab' and 'ylab' are not handled.")

          ## Add title if necessary
          if(!is.null(sub)||!is.null(main))
              title(main, sub, cex.main=cex.main, col.main=col.main,
                    cex.sub=cex.sub, col.sub=col.sub)




          ## Draw Nodes
          node.info <- x@nodeInfo # FIXME: write accessor
          drawNodes(node.info, nodes(x)) 

          ## Use the smallest node radius as a means to scale the size of
          ## the arrowheads -- in INCHES! see man page for "arrows", which is called
          ## from bLines, which is called from lines.
          minDim <-
              min(getLayoutPar("rWidth", node.info) + getLayoutPar("lWidth", node.info),
                  getLayoutPar("height", node.info))
          arrowLen <- par("pin")[1] / diff(par("usr")[1:2]) * minDim / pi

          ## Draw Edges
          edge.info <- x@edgeInfo
          drawEdges(edge.info, len = arrowLen, edgemode = edgemode(x))
          return(invisible(x))
      })


