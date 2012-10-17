## need to redefine some of the getters for vectorization
getLabelPos <- function(f, slot = "x")
{
    ans <- slot(labelLoc(txtLabel(f)), slot)
    if (length(ans) == 0) NA_real_
    else ans
}

getLabelJust <- function(f)
{
    ans <- labelJust(txtLabel(f))
    if (length(ans) == 0) NA_character_
    else ans
}

getLabelWidth <- function(f)
{
    ans <- labelWidth(txtLabel(f))
    if (length(ans) == 0) NA_integer_
    else ans
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
nodeRagraph2graph <- function(g, x)
{
    ## get node locations from the Ragraph
    agn <- AgNode(g)
    rw <- sapply(agn, getNodeRW)
    lw <- sapply(agn, getNodeLW)
    height <- sapply(agn, getNodeHeight)
    centerX <- sapply(agn, function(f) getNodeCenter(f)@x)
    centerY <- sapply(agn, function(f) getNodeCenter(f)@y)

    ## get node label locations from the Ragraph (Note: They don't
    ## seem to be used, falling back to node locations for now)
    #labelX <- sapply(agn, getLabelPos, "x")
    #labelY <- sapply(agn, getLabelPos, "y")
    labelX <- centerX
    labelY <- centerY
    labelJust <- sapply(agn, getLabelJust)
    labelWidth <- sapply(agn, getLabelWidth)
    shape <- sapply(agn,shape) 
    style <- sapply(agn, style)
    col <- sapply(agn, color)
    fill <- sapply(agn, fillcolor)

    ans <- 
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
             style = style,
             col = col, fill = fill)
    for (i in names(ans)) names(ans[[i]]) <- nodes(x)
    ans
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
edgeRagraph2graph <- function(g, x)
{
   
    ## get edge locations from the Ragraph
    age <- AgEdge(g)
    enamesFrom <- sapply(age, slot, "tail")
    enamesTo <- sapply(age, slot, "head")
    splines <- lapply(age, splines)

    ## get edge label locations from the Ragraph
    labelX <- sapply(age, getLabelPos, "x")
    labelY <- sapply(age, getLabelPos, "y")
    labelJust <- sapply(age, getLabelJust)
    labelWidth <- sapply(age, getLabelWidth)

    ## get arrowhead information from the Ragraph
    arrowhead <- sapply(age, arrowhead)
    arrowtail <- sapply(age, arrowtail)
    dir <- sapply(age, slot, "dir")

    col <- sapply(age, color)
    ans <- 
        list(enamesFrom = enamesFrom,
             enamesTo = enamesTo,
             splines = splines,
             labelX = labelX,
             labelY = labelY,
             labelJust = labelJust,
             labelWidth = labelWidth,
             arrowhead = arrowhead,
             arrowtail = arrowtail,
             direction = dir,
             col = col)
    for (i in names(ans))
        names(ans[[i]]) <- paste(enamesFrom, enamesTo, sep="~")
    ans
}



## Grab the graph-wide information from an Ragraph
## and put it into graph's graphData
## This will extract:
##   - bbox: the bounding box of the graph
graphRagraph2graph <- function(g, x)
{
  ## get bounding box from Ragraph
  bbox <- matrix(c(g@boundBox@botLeft@x, g@boundBox@botLeft@y,
                   g@boundBox@upRight@x, g@boundBox@upRight@y),
                  ncol=2, byrow=T)

  ## fill graphData
  gr <- graphRenderInfo(x)
  gr$bbox <- bbox
  gr$laidout=TRUE
  graphRenderInfo(x) <- gr
  return(x)
}



## wrapper function to call layout engine on a graph object
layoutGraph <- function(x, layoutFun=layoutGraphviz, ...){
  if (!validGraph(x))
    stop("The graph to be plotted is not a valid graph structure")
  layoutFun(x, ...)
}



## Do graph layout using the API to the graphviz algorithms available
## through Rgraphviz by calling agopen only to get the edge and node
## coordinates and the label locations and save all layout information
## to the graph
layoutGraphviz <- function(x, layoutType="dot", name="graph",
                           recipEdges="combined", nodeAttrs=list(),
                           edgeAttrs=list(), attrs=list(), ...)
{
    if(numNodes(x) == 0) {
        warning("graph has zero nodes; cannot layout\n")
        return(x)
    }
    ## defaults can be passed in via the attrs argument and we want to
    ## set those first
    if(!is.null(attrs$node))
        nodeRenderInfo(x) <- attrs$node
    if(!is.null(attrs$edge))
        edgeRenderInfo(x) <- attrs$edge
    if(!is.null(attrs$graph))
        graphRenderInfo(x) <- attrs$graph
    
    ## pass along labels if present
    nodeLabels <- nodeRenderInfo(x, "label")
    if(!is.null(nodeLabels) && is.null(nodeAttrs$label) && is.null(attrs$node$label))
        nodeAttrs$label <- nodeLabels
    edgeLabels <- edgeRenderInfo(x, "label")
    if(!is.null(edgeLabels) && is.null(edgeAttrs$label) && is.null(attrs$edge$label))
        edgeAttrs$label <- edgeLabels

    ## make sure that arrowheads are passed on to graphviz
    ## but only if they are not functions
    edgeHead <- edgeHeadOrig <-
        if(length(edgeAttrs$arrowhead)) edgeAttrs$arrowhead else edgeRenderInfo(x, "arrowhead")
    hsel <- FALSE
    if(!is.null(edgeHead)){
        hsel <- sapply(edgeHead, is.function)
        edgeHead[hsel] <- "open"
        edgeAttrs$arrowhead <- edgeHead
    }
    tsel <- FALSE
    edgeTail <- edgeTailOrig <-
        if(length(edgeAttrs$arrowtail)) edgeAttrs$arrowtail else edgeRenderInfo(x, "arrowtail")
    if(!is.null(edgeTail)){
        tsel <- sapply(edgeTail, is.function)
        edgeTail[tsel] <- "open"
        edgeAttrs$arrowtail <- edgeTail
    }
    
    ## pass fontsize
    fontsize <- getRenderPar(x, "fontsize")
    names(fontsize) <- nodes(x)
    nodeAttrs$fontsize <- fontsize

    ## The node shapes, only pass them on if they are not functions
    shapes <- getRenderPar(x, "shape", "nodes")
    widths <- getRenderPar(x, "iwidth", "nodes")
    heights <- getRenderPar(x, "iheight", "nodes")
    names(shapes) <- names(widths) <- names(heights) <- nodes(x)
    attrShapes <- nodeAttrs$shape
    if(!is.null(attrShapes))
        shapes[names(attrShapes)] <- attrShapes
    shapesOrig <- shapes
    ssel <- FALSE
    if(!is.null(shapes)){
        ssel <- sapply(shapes, is.function)
        shapes[ssel] <- "circle"
        nodeAttrs$shape <- unlist(shapes)
    }

    #oshape <- shapes
    #circ <- shapes=="circle"
    #ell <- shapes=="ellipse"

    attrWidths <- nodeAttrs$width
    attrHeights <- nodeAttrs$height
    if(!is.null(attrWidths)) widths[names(attrWidths)] <- attrWidths
    if(!is.null(attrHeights)) heights[names(attrHeights)] <- attrHeights
    nodeAttrs$width <- widths
    nodeAttrs$height <- heights
    g <- agopen(x, name=name, layoutType=layoutType,
                recipEdges=recipEdges, nodeAttrs=nodeAttrs,
                edgeAttrs=edgeAttrs, attrs=attrs, ...)
    x <- graphRagraph2graph(g,x)
    graphRenderInfo(x) <- list(recipEdges=recipEdges)
    ## get information from the Ragraph object. Only replace labels if 
    ## they have been  explicitely specified in the edgeAttrs or nodeAttrs
    nri <- nodeRagraph2graph(g, x)
    ## nri$shape <- oshape
    if(!is.null(nodeAttrs$label))
        nri$label <- nodeAttrs$label
    if(any(ssel))
        nri$shape[names(which(ssel))] <- shapesOrig[ssel]
    nodeRenderInfo(x) <- nri
    eri <- edgeRagraph2graph(g, x)
    if(!is.null(edgeAttrs$label))
        eri$label <- edgeAttrs$label
    if(any(hsel))
        eri$arrowhead[names(which(hsel))] <- edgeHeadOrig[hsel]
    if(any(tsel))
        eri$arrowtail[names(which(tsel))] <- edgeTailOrig[tsel]
    edgeRenderInfo(x) <- eri
    return(invisible(x))
}


