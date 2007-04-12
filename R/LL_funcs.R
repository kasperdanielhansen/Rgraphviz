# TODO: 
# -- ?validate given nodes/edges
# -- ?check attrname/attrval/defaultval
# -- cluster defaults: specify cluster number or not
# -- cluster number: starting from 0 or 1

getDefAttrsGraph <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")

   ans <- .Call("Rgraphviz_getDefAttrsGraph", graph, PACKAGE="Rgraphviz")
   
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
	    PACKAGE="Rgraphviz")
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
      r <- .Call("Rgraphviz_getAttrsGraph", graph, attrname[i], PACKAGE="Rgraphviz")
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
	    PACKAGE="Rgraphviz")
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
                 graph, as.integer(x[i, 1]), x[i, 2], PACKAGE="Rgraphviz")
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
	     PACKAGE="Rgraphviz")
   }
}

getDefAttrsNode <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")

   ans <- .Call("Rgraphviz_getDefAttrsNode", graph, PACKAGE="Rgraphviz")

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
	    PACKAGE="Rgraphviz")
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
                 PACKAGE="Rgraphviz")
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
	    PACKAGE="Rgraphviz")
   }
}

getDefAttrsEdge <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")

   ans <- .Call("Rgraphviz_getDefAttrsEdge", graph, PACKAGE="Rgraphviz")

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
	    PACKAGE="Rgraphviz")
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
                 PACKAGE="Rgraphviz")
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
	    PACKAGE="Rgraphviz")
   }
}

toFile <- function(graph, 
		      layoutType=c("dot","neato","twopi","circo","fdp"), 
		      filename, 
		      fileType=c("canon", "dot", "xdot", "dia", "fig", 
				  "gd", "gd2", "gif", "hpgl", "imap", "cmapx", 
				  "ismap", "mif", "mp", "pcl", "pic", 
				  "plain", "plain-ext", "png", "ps", "ps2", 
				  "svg", "svgz", "vrml", "vtx", "wbmp")) 
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   
   layoutType <- match.arg(layoutType)
   fileType <- match.arg(fileType)
   # error check

   .Call("Rgraphviz_toFile", graph, 
		as.character(layoutType), 
		as.character(filename), 
		as.character(fileType), 
		PACKAGE="Rgraphviz")

   # msg for users
}

# graph: graphNEL
# name: string
# kind: int
# subGList: list of subgraphs
# recipEdges: combine reciprocal directed edges or not 
agopenSimple <- function(graph, name, 
		kind=NULL, edgeMode=edgemode(graph),
                subGList=list(), 
                recipEdges=c("combined", "distinct")) 
{
    if (!is(graph,"graph"))
        stop("This function is for objects of class 'graph' only")

    if (is.null(kind)) {
        ## Determine kind from the graph object
        outK <- switch(edgeMode,
                       "undirected"=0,  ## AGRAPH
                       "directed"=1,    ## AGDIGRAPH
                       0)
    }
    else {
        ## Use the specified 'kind' parameter.
        outK <- switch(kind,
                       "AGRAPH"=0,   ##Undirected Graph
                       "AGDIGRAPH"=1,   ## directed graph
                       "AGRAPHSTRICT"=2,   ## no self arcs or multiedges
                       "AGDIGRAPHSTRICT"=3, ## directed strict graph
                       stop(paste("Incorrect kind parameter:",kind)))
    }

    recipEdges <- match.arg(recipEdges)
    recipK <- switch(recipEdges,
                   "combined"=0,  
                   "distinct"=1, 
                    0)

    nsubG = length(subGList)

    sgi = vector(mode="numeric", length=numNodes(graph))
    sgi[] = nsubG 
    names(sgi) = nodes(graph)
    if ( nsubG > 0 ) 
	for ( i in 1:nsubG ) sgi[nodes(subGList[[i]]$graph)] = i

    g <- .Call("Rgraphviz_agopenSimple", name, as.integer(outK), 
		nodes(graph), as.integer(sgi),
		as.integer(edgeMatrix(graph)["from",]), 
		as.integer(edgeMatrix(graph)["to",]), 
		as.integer(nsubG), as.list(subGList),
		as.integer(recipK), 
                PACKAGE="Rgraphviz")
    g@edgemode <- edgeMode
    g@layoutType = "dot"    # default layout type

    g
}

