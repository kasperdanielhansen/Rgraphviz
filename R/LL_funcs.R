# TODO: 
# -- check given graph is Ragraph
# -- validate given nodes/edges
# -- check attrname/attrval/defaultval

## to enable: 	nodeAttr and nodeAttr <- x, 
##		edgeAttr and edgeAttr <- x
##setGeneric("AgNodeAttr", function(object) standardGeneric("AgNodeAttr"))
##setMethod("AgNodeAttr", "Ragraph", function(object) object@AgNode)
##
##setGeneric("AgNodeAttr<-", function(object, value) standardGeneric("AgNodeAttr<-"))
##setReplaceMethod("AgNodeAttr", "Ragraph", function(object, value) {
##                   object@AgNode = value
##                   return(object)
##                 })

LLgetDefAttrsGraph <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")

   ans <- .Call("Rgraphviz_getDefAttrsGraph", graph, PACKAGE="Rgraphviz")
   colnames(ans) <- c("attr name", "attr value")
   rownames(ans) <- paste("graph attr", 1:nrow(ans))
   ans
}

LLsetDefAttrsGraph <- function(graph, attrnames=c(), attrvals=c())
{
   if ( length(attrnames) != length(attrvals) )
      stop("Length of attrnames is not equal to length of attrvals")

   nattr = length(attrnames)

   ans <- .Call("Rgraphviz_setDefAttrsGraph", 
	 	graph, as.integer(nattr), attrnames, attrvals, 
		PACKAGE="Rgraphviz")
}

LLgetAttrsGraph <- function(graph, attrname)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || attrname=="" ) 
	stop("attrname is needed")

   .Call("Rgraphviz_getAttrsGraph", graph, 
	attrname, PACKAGE="Rgraphviz")
}

LLsetAttrsGraph <- function(graph, attrname, attrval, defaultval="")
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || attrname=="" ) 
	stop("attrname is needed")
   if ( missing(attrval) || !is.character(attrval) ) 
	stop("attrval is needed")

   .Call("Rgraphviz_setAttrsGraph", graph, 
	attrname, attrval, defaultval, 
	PACKAGE="Rgraphviz")
}

LLgetDefAttrsCluster <- function(graph, cluster)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( !is.numeric(cluster) ) stop("Cluster is not given as an integer")

   ans <- .Call("Rgraphviz_getDefAttrsCluster", 
		graph, as.integer(cluster), 
		PACKAGE="Rgraphviz")

   if ( !is.null(ans) )
   {
      colnames(ans) <- c("attr name", "attr value")
      rownames(ans) <- paste("cluster attr", 1:nrow(ans))
   }

   ans
}

LLsetDefAttrsCluster <- function(graph, cluster, attrnames=c(), attrvals=c())
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( !is.numeric(cluster) ) stop("Cluster is not given as an integer")

   if ( length(attrnames) != length(attrvals) )
      stop("Length of attrnames is not equal to length of attrvals")

   nattr = length(attrnames)

   ans <- .Call("Rgraphviz_setDefAttrsCluster", 
	 	graph, as.integer(cluster), 
		as.integer(nattr), attrnames, attrvals, 
		PACKAGE="Rgraphviz")

   ans
}

LLgetAttrsCluster <- function(graph, cluster, attrname)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( !is.numeric(cluster) ) stop("Cluster is not given as an integer")

   if ( missing(attrname) || !is.character(attrname) || attrname=="" ) 
	stop("attrname is needed")

   ans <- .Call("Rgraphviz_getAttrsCluster", 
		graph, as.integer(cluster), attrname, 
		PACKAGE="Rgraphviz")
   ans
}

LLsetAttrsCluster <- function(graph, cluster, attrname, attrval, defaultval="")
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( !is.numeric(cluster) ) stop("Cluster is not given as an integer")

   if ( missing(attrname) || !is.character(attrname) || attrname=="" ) 
	stop("attrname is needed")
   if ( missing(attrval) || !is.character(attrval) ) 
	stop("attrval is needed")

   ans <- .Call("Rgraphviz_setAttrsCluster", 
		graph, as.integer(cluster),
		attrname, attrval, defaultval, 
		PACKAGE="Rgraphviz")
   ans
}

LLgetDefAttrsNode <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")

   ans <- .Call("Rgraphviz_getDefAttrsNode", graph, PACKAGE="Rgraphviz")

   colnames(ans) <- c("attr name", "attr value")
   rownames(ans) <- paste("node attr", 1:nrow(ans))
       
   ans
}

LLsetDefAttrsNode <- function(graph, attrnames=c(), attrvals=c())
{
   if ( length(attrnames) != length(attrvals) )
      stop("Length of attrnames is not equal to length of attrvals")

   nattr = length(attrnames)
   ans <- .Call("Rgraphviz_setDefAttrsNode", 
		graph, as.integer(nattr), attrnames, attrvals,
		PACKAGE="Rgraphviz")
}

LLgetAttrsNode <- function(graph, node, attrname)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || any(attrname=="") ) 
	stop("attrname is needed")

   x <- cbind(node, attrname)

   ans <- vector()
   for ( i in 1:nrow(x) )
   {
      r <- .Call("Rgraphviz_getAttrsNode", graph, x[i, 1], 
		x[i, 2], PACKAGE="Rgraphviz")
      if ( is.null(r) ) r <- "ERROR"
      names(r) <- x[i, 1]
      ans <- c(ans, r)
   }

   ans
}

LLsetAttrsNode <- function(graph, node, attrname, attrval, defaultval="")
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || attrname=="" ) 
	stop("attrname is needed")
   if ( missing(attrval) || !is.character(attrval) ) 
	stop("attrval is needed")

   x <- cbind(node, attrname, attrval, defaultval)

   for ( i in 1:nrow(x) )
   {
   .Call("Rgraphviz_setAttrsNode", graph, x[i, 1], 
	x[i, 2], x[i, 3], x[i, 4],
	PACKAGE="Rgraphviz")
   }
}

LLgetDefAttrsEdge <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")

   ans <- .Call("Rgraphviz_getDefAttrsEdge", graph, PACKAGE="Rgraphviz")

   colnames(ans) <- c("attr name", "attr value")
   rownames(ans) <- paste("edge attr", 1:nrow(ans))
       
   ans
}

LLsetDefAttrsEdge <- function(graph, attrnames=c(), attrvals=c())
{
   if ( length(attrnames) != length(attrvals) )
      stop("Length of attrnames is not equal to length of attrvals")

   nattr = length(attrnames)

   ans <- .Call("Rgraphviz_setDefAttrsEdge", 
		graph, as.integer(nattr), attrnames, attrvals,
		PACKAGE="Rgraphviz")
}

LLgetAttrsEdge <- function(graph, from, to, attrname)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || attrname=="" ) 
	stop("attrname is needed")

   x <- cbind(from, to, attrname)

   ans <- vector()

   for ( i in 1:nrow(x) )
   {
      r <- .Call("Rgraphviz_getAttrsEdge", graph, x[i, 1], x[i, 2], 
	   	   x[i, 3], PACKAGE="Rgraphviz")
      if ( is.null(r) ) r <- "ERROR"
      names(r) <- paste(x[i, 1], "--", x[i, 2], sep="")
      ans <- c(ans, r)
   }

   ans
}

LLsetAttrsEdge <- function(graph, from, to, attrname, attrval, defaultval="") 
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   if ( missing(attrname) || !is.character(attrname) || attrname=="" ) 
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

LLgetDefAttrs <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   
   ans_g <- LLgetDefAttrsGraph(graph)
   #ans_c <- LLgetDefAttrsCluster(graph)  # which one to inquire?
   ans_n <- LLgetDefAttrsNode(graph)
   ans_e <- LLgetDefAttrsEdge(graph)

   ans <- rbind(ans_g, ans_n, ans_e)

   ans
}

LLsetDefAttrs <- function(graph, g_attrnames=c(), g_attrvals=c(),
			#c_attrnames=c(), c_attrvals=c(),
			n_attrnames=c(), n_attrvals=c(),
			e_attrnames=c(), e_attrvals=c())
{
   ans_g <- LLsetDefAttrsGraph(graph, g_attrnames, g_attrvals)
   #ans_c <- LLsetDefAttrsCluster(graph, c_attrnames, c_attrvals)
   ans_n <- LLsetDefAttrsNode(graph, n_attrnames, n_attrvals)
   ans_e <- LLsetDefAttrsEdge(graph, e_attrnames, e_attrvals)
}

LLtoFile <- function(graph, 
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
LLagopen <- function(graph, name, 
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

    g <- .Call("LLagopen", name, as.integer(outK), 
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

