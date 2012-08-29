replaceAttrs <- function(attributes, what, which, with)
{
    if (is.null(attributes[[what]][[which]]))
        attributes[[what]][[which]] <- with
    attributes
}


getDefaultAttrs <- function(curAttrs=list(),
                            layoutType = graphvizCapabilities()$layoutTypes) {
    layoutType <- match.arg(layoutType)
    if (length(curAttrs) == 0)
        curAttrs <- list(graph=list(), cluster=list(),
                         node=list(), edge=list())
    else {
        if (is.null(curAttrs$graph)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "graph"
        }
        if (is.null(curAttrs$cluster)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "cluster"
        }
        if (is.null(curAttrs$node)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "node"
        }
        if (is.null(curAttrs$edge)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "edge"
        }
    }

    bg <- "transparent"
    fg <- "black"
    col <- "black"

    ## Define the graph attributes
    curAttrs <- replaceAttrs(curAttrs, "graph", "bgcolor", bg)
    curAttrs <- replaceAttrs(curAttrs, "graph", "fontcolor", fg)
    curAttrs <- replaceAttrs(curAttrs, "graph", "ratio", "fill")
    curAttrs <- replaceAttrs(curAttrs, "graph", "overlap", "")
    curAttrs <- replaceAttrs(curAttrs, "graph", "splines", "TRUE")
    curAttrs <- replaceAttrs(curAttrs, "graph", "rank", "same")
   
    ## Use the 'fin' value for the Graphviz size, if there's no
    ## plot device open right now, then use a sensible default
    ## instead of letting Graphviz choose whatever it wants.  This
    ## helps prevent visual distortion when scaling down the image.
    if(is.null(curAttrs$graph$size)) {
        ## we need to set the size
        ## if a device is open, use that size, otherwise default to 7,7
        if (.Device != "null device"){
            curAttrs <- replaceAttrs(curAttrs, "graph", "size",
                                   paste(par("fin")[1], par("fin")[2], sep= ","))
        } else {
            curAttrs <- replaceAttrs(curAttrs, "graph", "size", "6.99,6.99")
        }
    }

    ## Now do layout specific graph attributes
    if (layoutType == "dot")
        curAttrs <- replaceAttrs(curAttrs, "graph", "rankdir", "TB")
     

    ## Now do cluster attributes
    curAttrs <- replaceAttrs(curAttrs, "cluster", "bgcolor", bg)
    curAttrs <- replaceAttrs(curAttrs, "cluster", "color", col)
    curAttrs <- replaceAttrs(curAttrs, "cluster", "rank", "same")
    
   
    ## node attributes
    curAttrs <- replaceAttrs(curAttrs, "node", "shape", "circle")
    curAttrs <- replaceAttrs(curAttrs, "node", "fixedsize", TRUE)
    curAttrs <- replaceAttrs(curAttrs, "node", "fillcolor", bg)
    curAttrs <- replaceAttrs(curAttrs, "node", "label", "\\N")
    curAttrs <- replaceAttrs(curAttrs, "node", "color", col)
    curAttrs <- replaceAttrs(curAttrs, "node", "fontcolor", fg)
    curAttrs <- replaceAttrs(curAttrs, "node", "fontsize", "14")
    curAttrs <- replaceAttrs(curAttrs, "node", "height", "0.5")
    curAttrs <- replaceAttrs(curAttrs, "node", "width", "0.75")
    curAttrs <- replaceAttrs(curAttrs, "node", "border.lwd", 1)
    curAttrs <- replaceAttrs(curAttrs, "node", "border.color", 'black')
    #curAttrs <- replaceAttrs(curAttrs, "node", "style", "solid")
    #curAttrs <- replaceAttrs(curAttrs, "node", "distortion", "0.0")
    #curAttrs <- replaceAttrs(curAttrs, "node", "layer", "solid")
    #curAttrs <- replaceAttrs(curAttrs, "node", "regular", "0.0")
    #curAttrs <- replaceAttrs(curAttrs, "node", "sides", "4")
    #curAttrs <- replaceAttrs(curAttrs, "node", "skew", "0.0")
   

   
    ## edge attrs
    curAttrs <- replaceAttrs(curAttrs, "edge", "color", col)
    curAttrs <- replaceAttrs(curAttrs, "edge", "dir", "none")
    curAttrs <- replaceAttrs(curAttrs, "edge", "weight", "1.0")
    curAttrs <- replaceAttrs(curAttrs, "edge", "label", "")
    curAttrs <- replaceAttrs(curAttrs, "edge", "fontcolor", fg)
    curAttrs <- replaceAttrs(curAttrs, "edge", "arrowhead", "none")
    curAttrs <- replaceAttrs(curAttrs, "edge", "arrowtail", "none")
    curAttrs <- replaceAttrs(curAttrs, "edge", "fontsize", "14")
    curAttrs <- replaceAttrs(curAttrs, "edge", "labelfontsize", "11")
    curAttrs <- replaceAttrs(curAttrs, "edge", "arrowsize", "1")
    curAttrs <- replaceAttrs(curAttrs, "edge", "headport", "center")
    curAttrs <- replaceAttrs(curAttrs, "edge", "layer", "")
    curAttrs <- replaceAttrs(curAttrs, "edge", "style", "solid")
   
   
    ## Now do layout specific edge attributes
    if (layoutType == "dot") {
        #curAttrs <- replaceAttrs(curAttrs, "edge", "constraint", FALSE)
        curAttrs <- replaceAttrs(curAttrs, "edge", "minlen", "1")
    }

    if (layoutType == "neato")
        curAttrs <- replaceAttrs(curAttrs, "edge", "len", "1.0")
       

    return(curAttrs)
}


checkAttrs <- function(attrs) {
    if (!is.list(attrs))
        stop("attrs must be a list")
    if (length(attrs) != 4)
        stop("attrs must be of length 4")
    if (!all(names(attrs) %in%
             c("graph","cluster", "node","edge")))
        stop(paste("Names of attrs must be 'graph',",
                   "'cluster', 'node', and 'edge'"))
    TRUE
}


makeNodeAttrs <- function(g, label = nodes(g), shape="ellipse", fillcolor = "#e0e0e0",
                          border.lwd = 1, border.color = "black", ...) {
    rv <- list(label = label, shape = shape, fillcolor = fillcolor,
               border.lwd = border.lwd, border.color = border.color, ...)
    for(i in seq(along=rv)) {
        if(length(rv[[i]]) == 1) {
            rv[[i]] <- rep(rv[[i]], numNodes(g))
        } else {
            if(length(rv[[i]]) != numNodes(g))
                stop("Attribute vector must have as many elements as 'g' has nodes.")
        }
        names(rv[[i]]) <- nodes(g)
    }
    return(rv)
}

## TODO: 
## -- ?validate given nodes/edges
## -- ?check attrname/attrval/defaultval
## -- cluster defaults: specify cluster number or not
## -- cluster number: starting from 0 or 1

getDefAttrsGraph <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")

   ans <- .Call("Rgraphviz_getDefAttrsGraph", graph, PACKAGE="Rgraphviz2")
   
   if ( !is.null(ans) && nrow(ans) > 0 )
   {
      colnames(ans) <- c("attr name", "attr value")
      rownames(ans) <- paste("graph attr", 1:nrow(ans))
   }
   ans
}

setDefAttrsGraph <- function(graph, attrnames=c(), attrvals=c())
{
   if ( length(attrnames) != length(attrvals) )
      stop("Length of attrnames is not equal to length of attrvals")

   x <- cbind(attrnames, attrvals)

   for ( i in 1:nrow(x) )
   {
      .Call("Rgraphviz_setDefAttrsGraph", graph, x[i, 1], x[i, 2], 
	    PACKAGE="Rgraphviz2")
   }
}

getAttrsGraph <- function(graph, attrname)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || any(attrname=="") ) 
	stop("attrname is needed")

   ans <- vector(length=length(attrname))
   names(ans) <- attrname
   for ( i in 1:length(attrname) )
   {
      r <- .Call("Rgraphviz_getAttrsGraph", graph, attrname[i], PACKAGE="Rgraphviz2")
      if ( !is.null(r) ) ans[i] <- r
   }

   ans
}

setAttrsGraph <- function(graph, attrname, attrval, defaultval="")
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || any(attrname=="") ) 
	stop("attrname is needed")
   if ( missing(attrval) || !is.character(attrval) ) 
	stop("attrval is needed")

   x <- cbind(attrname, attrval, defaultval)

   for ( i in 1:nrow(x) )
   {
      .Call("Rgraphviz_setAttrsGraph", graph, x[i, 1], x[i, 2], x[i, 3],
	    PACKAGE="Rgraphviz2")
   }
}

getAttrsCluster <- function(graph, cluster, attrname)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( !is.numeric(cluster) ) stop("Cluster is not given as an integer")

   if ( missing(attrname) || !is.character(attrname) || any(attrname=="") ) 
	stop("attrname is needed")

   x <- cbind(cluster, attrname)

   ans <- vector(length=nrow(x))
   names(ans) <- x[, 1]
   for ( i in 1:nrow(x) )
   {
      r <- .Call("Rgraphviz_getAttrsCluster", 
                 graph, as.integer(x[i, 1]), x[i, 2], PACKAGE="Rgraphviz2")
      if ( !is.null(r) ) ans[i] <- r
   }

   ans
}

setAttrsCluster <- function(graph, cluster, attrname, attrval, defaultval="")
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( !is.numeric(cluster) ) stop("Cluster is not given as an integer")

   if ( missing(attrname) || !is.character(attrname) || any(attrname=="") )
        stop("attrname is needed")
   if ( missing(attrval) || !is.character(attrval) )
        stop("attrval is needed")

   x <- cbind(cluster, attrname, attrval, defaultval)

   for ( i in 1:nrow(x) )
   {
       .Call("Rgraphviz_setAttrsCluster", 
             graph, as.integer(x[i, 1]), x[i, 2], x[i, 3], x[i, 4],
	     PACKAGE="Rgraphviz2")
   }
}

getDefAttrsNode <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")

   ans <- .Call("Rgraphviz_getDefAttrsNode", graph, PACKAGE="Rgraphviz2")

   if ( !is.null(ans) && nrow(ans) > 0 )
   {
      colnames(ans) <- c("attr name", "attr value")
      rownames(ans) <- paste("node attr", 1:nrow(ans))
   }
       
   ans
}

setDefAttrsNode <- function(graph, attrnames=c(), attrvals=c())
{
   if ( length(attrnames) != length(attrvals) )
      stop("Length of attrnames is not equal to length of attrvals")

   x <- cbind(attrnames, attrvals)

   for ( i in 1:nrow(x) )
   {
      .Call("Rgraphviz_setDefAttrsNode", graph, x[i, 1], x[i, 2],
	    PACKAGE="Rgraphviz2")
   }
}

getAttrsNode <- function(graph, node, attrname)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || any(attrname=="") ) 
	stop("attrname is needed")

   x <- cbind(node, attrname)

   ans <- vector(length=nrow(x))
   names(ans) <- x[, 1]
   for ( i in 1:nrow(x) )
   {
      r <- .Call("Rgraphviz_getAttrsNode", graph, x[i, 1], x[i, 2], 
                 PACKAGE="Rgraphviz2")
      if ( !is.null(r) ) ans[i] <- r
   }

   ans
}

setAttrsNode <- function(graph, node, attrname, attrval, defaultval="")
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || any(attrname=="") ) 
	stop("attrname is needed")
   if ( missing(attrval) || !is.character(attrval) ) 
	stop("attrval is needed")

   x <- cbind(node, attrname, attrval, defaultval)

   for ( i in 1:nrow(x) )
   {
      .Call("Rgraphviz_setAttrsNode", graph, x[i, 1], x[i, 2], x[i, 3], x[i, 4],
	    PACKAGE="Rgraphviz2")
   }
}

getDefAttrsEdge <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")

   ans <- .Call("Rgraphviz_getDefAttrsEdge", graph, PACKAGE="Rgraphviz2")

   if ( !is.null(ans) && nrow(ans) > 0 )
   {
   colnames(ans) <- c("attr name", "attr value")
   rownames(ans) <- paste("edge attr", 1:nrow(ans))
   }
       
   ans
}

setDefAttrsEdge <- function(graph, attrnames=c(), attrvals=c())
{
   if ( length(attrnames) != length(attrvals) )
      stop("Length of attrnames is not equal to length of attrvals")

   x <- cbind(attrnames, attrvals)

   for ( i in 1:nrow(x) )
   {
      .Call("Rgraphviz_setDefAttrsEdge", graph, x[i, 1], x[i, 2],
	    PACKAGE="Rgraphviz2")
   }
}

getAttrsEdge <- function(graph, from, to, attrname)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || any(attrname=="") ) 
	stop("attrname is needed")

   x <- cbind(from, to, attrname)

   ans <- vector(length=nrow(x))
   names(ans) <- paste(x[, 1], x[, 2], sep="~")
   for ( i in 1:nrow(x) )
   {
      r <- .Call("Rgraphviz_getAttrsEdge", graph, x[i, 1], x[i, 2], x[i, 3], 
                 PACKAGE="Rgraphviz2")
      if ( !is.null(r) ) ans[i] <- r
   }

   ans
}

setAttrsEdge <- function(graph, from, to, attrname, attrval, defaultval="") 
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || any(attrname=="") ) 
	stop("attrname is needed")
   if ( missing(attrval) || !is.character(attrval) ) 
	stop("attrval is needed")
   
   x <- cbind(from, to, attrname, attrval, defaultval)

   for ( i in 1:nrow(x) )
   {
       .Call("Rgraphviz_setAttrsEdge", graph, x[i, 1], x[i, 2], 
	    x[i, 3], x[i, 4], x[i, 5], 
	    PACKAGE="Rgraphviz2")
   }
}

