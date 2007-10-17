## Wrapper around paste for simple yet flexible namespacing of the attribute
## names. Is set to do nothing for now
myAtt <- function(att) att



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
    nnames <- sapply(agn, slot, "name")
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
    #label <- sapply(agn, function(f) labelText(txtLabel(f)))
    
    ## FIXME?: agopen should have shape=ellipse when layouttype=dot,
    ## but seems to give circle.  So, we're going to ignore agn@shape
    ## and use g@layoutType instead. Should this be set on all node
    ## then? What if someone wants to use different shapes for
    ## different nodes? Would make more more sense to dynamically set
    ## this as a graph.par
    shape <- rep(if (g@layoutType == "dot") "ellipse" else "circle", length(nnames))
    style <- sapply(agn, style)

    ans <- 
        list(rWidth = rw, 
             lWidth = lw, 
             height = height, 
             nodeX = centerX, 
             nodeY = centerY,
             #label=label,
             labelX = labelX, 
             labelY = labelY,
             labelJust = labelJust, 
             labelWidth = labelWidth,
             shape = shape,
             style = style)
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
    label <- sapply(age, function(f) labelText(txtLabel(f)))
    labelX <- sapply(age, getLabelPos, "x")
    labelY <- sapply(age, getLabelPos, "y")
    labelJust <- sapply(age, getLabelJust)
    labelWidth <- sapply(age, getLabelWidth)

    ## get arrowhead information from the Ragraph
    arrowhead <- sapply(age, arrowhead)
    arrowtail <- sapply(age, arrowtail)
    dir <- sapply(age, slot, "dir")
    
    ans <- 
        list(enamesFrom = enamesFrom,
             enamesTo = enamesTo,
             splines = splines,
             label=label,
             labelX = labelX,
             labelY = labelY,
             labelJust = labelJust,
             labelWidth = labelWidth,
             arrowhead = arrowhead,
             arrowtail = arrowtail,
             direction = dir)
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
                           recipEdges="combined", ...)
{
    g <- agopen(x, name=name, layoutType=layoutType,
                recipEdges=recipEdges, ...)
    x <- graphRagraph2graph(g,x)
    graphRenderInfo(x) <- list(recipEdges=recipEdges)
    nodeRenderInfo(x) <- nodeRagraph2graph(g, x)
    edgeRenderInfo(x) <- edgeRagraph2graph(g, x)
    return(invisible(x))
  }



