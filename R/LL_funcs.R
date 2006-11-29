# TODO: 
# -- check given graph is Ragraph
# -- validate given nodes/edges
# -- check attrname/attrval/defaultval

LLgetDefAttrsGraph <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")

   ans <- .Call("Rgraphviz_getDefAttrsGraph", graph, PACKAGE="Rgraphviz")

   colnames(ans) <- c("attr name", "attr value")
   rownames(ans) <- paste("graph attr", 1:nrow(ans))

   ans
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

LLgetDefAttrsNode <- function(graph)
{
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")

   ans <- .Call("Rgraphviz_getDefAttrsNode", graph, PACKAGE="Rgraphviz")

   colnames(ans) <- c("attr name", "attr value")
   rownames(ans) <- paste("node attr", 1:nrow(ans))

   ans
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
   ans_n <- LLgetDefAttrsNode(graph)
   ans_e <- LLgetDefAttrsEdge(graph)

   ans <- rbind(ans_g, ans_n, ans_e)

   ans
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
   
   which1 <- match.arg(layoutType)
   which2 <- match.arg(fileType)
   # error check

   .Call("Rgraphviz_toFile", graph, 
		as.character(layoutType), 
		as.character(filename), 
		as.character(fileType), 
		PACKAGE="Rgraphviz")

   # msg for users
}

