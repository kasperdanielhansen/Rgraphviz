setMethod("getX", "xyPoint", function(object) object@x)
setMethod("getY", "xyPoint", function(object) object@y)
setMethod("getPoints", "xyPoint", function(object) c(object@x, object@y))
setMethod("show", "xyPoint", function(object)
          cat(paste("x: ", object@x, ", y: ", object@y, "\n", sep="")))

setMethod("labelText", "AgTextLabel", function(object) object@labelText)
setMethod("labelColor", "AgTextLabel", function(object) object@labelColor)
setMethod("labelLoc", "AgTextLabel", function(object) object@labelLoc)
setMethod("labelJust", "AgTextLabel", function(object) object@labelJust)
setMethod("labelWidth","AgTextLabel", function(object) object@labelWidth)
setMethod("labelFontsize", "AgTextLabel", function(object) object@labelFontsize)

setMethod("botLeft", "boundingBox", function(object) object@botLeft)
setMethod("upRight", "boundingBox", function(object) object@upRight)

setMethod("shape", "AgNode", function(object) object@shape)
setMethod("style", "AgNode", function(object) object@style)
setMethod("color","AgNode", function(object) object@color)
setMethod("fillcolor", "AgNode", function(object) object@fillcolor)
setMethod("getNodeCenter", "AgNode", function(object) object@center)
setMethod("getNodeHeight", "AgNode", function(object) object@height)
setMethod("getNodeRW", "AgNode", function(object) object@rWidth)
setMethod("getNodeLW", "AgNode", function(object) object@lWidth)
setMethod("name", "AgNode", function(object) object@name)
setMethod("txtLabel", "AgNode", function(object) object@txtLabel)
setMethod("getNodeXY", "AgNode", function(object) {
    cen <- getNodeCenter(object)
    out <- list(x=getX(cen), y=getY(cen))
    out
})

setMethod("color","AgEdge", function(object) object@color)
setMethod("arrowsize", "AgEdge", function(object) object@arrowsize)
setMethod("arrowhead", "AgEdge", function(object) object@arrowhead)
setMethod("arrowtail", "AgEdge", function(object) object@arrowtail)
setMethod("txtLabel", "AgEdge", function(object) object@txtLabel)
setMethod("splines", "AgEdge", function(object) object@splines)
setMethod("sp", "AgEdge", function(object) object@sp)
setMethod("ep", "AgEdge", function(object) object@ep)
setMethod("head", "AgEdge", function(x, ...) x@head)
setMethod("tail", "AgEdge", function(x, ...) x@tail)
setMethod("numSplines", "AgEdge", function(object) length(object@splines))
setMethod("getSpline", "AgEdge", function(object, pos) {
    if ((pos > 0)&&(pos <= numSplines(object)))
        return(object@splines[[pos]])
    else
        return(NULL)

})
setMethod("show", "AgEdge", function(object) {
    z <- splines(object)
    out <- paste("An edge between", head(object),
                 "and", tail(object),
                 "with",numSplines(object),
		 "BezierCurve objects:")
    cat(out,"\n")
    for (i in seq(along=z))
        show(z[[i]])
})


setMethod("cPoints", "BezierCurve", function(object) object@cPoints)
setMethod("pointList", "BezierCurve", function(object) {
    z <- cPoints(object)
    out <- lapply(z, getPoints)
    out
})
setMethod("bezierPoints", "BezierCurve", function(x) {
    z <- pointList(x)
    out <- vector("list", length=11)
    for (i in 0:10)
        out[[i+1]] <- .Call("Rgraphviz_bezier", z, length(z), i/10,
                            PACKAGE="Rgraphviz")
    out <- matrix(unlist(out), ncol=2, byrow=TRUE,
                  dimnames=list(NULL,c("x","y")))
    out
})
setMethod("bLines", "BezierCurve", 
		function(x,...,col=par("col"), len=0.25, lty=par("lty"),
                         lwd=par("lwd"), arrowtail="none", arrowhead="none") {
    .Deprecated("bLines", package="Rgraphviz")
    z <- bezierPoints(x)

    numSegs <- nrow(z)
    lines(z[2:(numSegs-1),1], z[2:(numSegs-1),2], col=col, lty=lty, lwd=lwd)

    tailStart <- z[2,]
    tailEnd <- z[1,]
    switch(arrowtail,
           "none"=lines(c(tailStart[1], tailEnd[1]),
                        c(tailStart[2], tailEnd[2]),
                        col=col, lty=lty, lwd=lwd),
           "open"=arrows(tailStart[1], tailStart[2], tailEnd[1], tailEnd[2],
                        col=col, length=len, lty=lty, lwd=lwd),
           stop("Unsupported arrowtail type: ", arrowtail))

    headStart <- z[numSegs-1,]
    headEnd <- z[numSegs,]

    switch(arrowhead,
           "none"=lines(c(headStart[1], headEnd[1]),
                        c(headStart[2], headEnd[2]),
                        col=col, lty=lty, lwd=lwd),
           "open"=arrows(headStart[1], headStart[2], headEnd[1], headEnd[2],
                        col=col, length=len, lty=lty, lwd=lwd),
           stop("Unsupported arrowhead type: ", arrowhead))
})
setMethod("show", "BezierCurve", function(object) {
    z <- cPoints(object)
    out <- paste(unlist(lapply(z,
                 function(x){paste(getPoints(x), collapse=",")})), collapse=" ")
    out <- paste(out,"\n")
    cat(out)
})




setMethod("agraph", "Ragraph", function(object) object@agraph)
setMethod("edgemode", "Ragraph", function(object) object@edgemode)
setMethod("laidout", "Ragraph", function(object) object@laidout)
setMethod("layoutType", "Ragraph", function(object) object@layoutType)
setMethod("boundBox", "Ragraph", function(object) object@boundBox)
##------------------------------------------------------------
## accessor and replacement methods for AgEdge slot
##------------------------------------------------------------
setMethod("AgEdge", "Ragraph", function(object) object@AgEdge)
setReplaceMethod("AgEdge", "Ragraph", function(object, value) {
                   object@AgEdge = value
                   return(object)
})
##------------------------------------------------------------
## accessor and replacement methods for AgNode slot
##------------------------------------------------------------
setMethod("AgNode", "Ragraph", function(object) object@AgNode)
setReplaceMethod("AgNode", "Ragraph", function(object, value) {
    object@AgNode = value
    return(object)
})
setMethod("getNodeXY", "Ragraph",  function(object) {
    out <- vector(mode="list",length=2)
    names(out) <- c("x","y")
    xys <- lapply(object@AgNode,getNodeCenter)
    out[[1]] <- unlist(lapply(xys,getX))
    out[[2]] <- unlist(lapply(xys,getY))
    out
})
setMethod("getNodeHeight", "Ragraph", function(object) {
      nodes = object@AgNode
      sapply(nodes, getNodeHeight)
   })
setMethod("getNodeRW", "Ragraph", function(object) {
      nodes = object@AgNode
      sapply(nodes, getNodeRW)
   })
setMethod("getNodeLW", "Ragraph", function(object) {
      nodes = object@AgNode
      sapply(nodes, getNodeLW)
   })
setMethod("show", "Ragraph", function(object) {
    print(paste("A graph with",length(AgNode(object)), "nodes."))
})
setMethod("edgeNames", "Ragraph",  function(object, recipEdges=c("combined", "distinct")) {
    recipEdges <- match.arg(recipEdges)
    LedgeNames <- sapply(AgEdge(object), 
                         function(x) paste(tail(x), head(x), sep="~"))
    
    if (recipEdges == "combined") {
        revNames <- sapply(AgEdge(object), 
			   function(x) paste(head(x), tail(x), sep="~"))
        
        handled <- character()
        remove <- numeric()
        for (i in 1:length(LedgeNames)) {
            if (! revNames[i] %in% handled)
                handled <- c(handled, LedgeNames[i])
            else
                remove <- c(remove, i)
        }
        if (length(remove) > 0) LedgeNames <- LedgeNames[-remove]
    }
    LedgeNames
})

setMethod("name", "pNode", function(object) object@name)

setMethod("from", "pEdge", function(object) object@from)
setMethod("to", "pEdge", function(object) object@to)


## lines
## FIXME: do we need this anymore?
setMethod("lines", "BezierCurve", function(x,...,col=par("col"), lty=par("lty"), lwd=par("lwd")) {
    z <- bezierPoints(x)
    lines(z[,1],z[,2],col=col,lty=lty,lwd=lwd)
})
setMethod("lines", "AgEdge", function(x, ..., len, lty=par("lty"), lwd=par("lwd")) {
    z <- splines(x)
    edgeColor <- color(x)
    if (edgeColor == "") edgeColor <- "black"
    
    arrowSize <- arrowsize(x)
    if ( arrowSize == "" ) arrowSize = "1"
    
    if(length(x@lty)>0) lty=x@lty[1]
    if(length(x@lwd)>0) lwd=x@lwd[1]
    
    len <- len * as.numeric(arrowSize)
    mapply(lines, z, MoreArgs=list(len=len, col=edgeColor, lty=lty, lwd=lwd, ...))
    
    ## TODO: arrow shapes should/could be from arrowtail/head 
    if ( x@dir == "both" || x@dir== "back" ) {
        tails = bezierPoints(z[[1]])
        tail_from = tails[2, ]
        tail_to   = tails[1, ]
        arrows(tail_from[1], tail_from[2], tail_to[1], tail_to[2],
               col=edgeColor, length=len, lty=lty, lwd=lwd)
    }
    if ( x@dir == "both" || x@dir == "forward" ) {
        heads = bezierPoints(z[[length(z)]])
        head_from = heads[nrow(heads)-1, ]
        head_to   = heads[nrow(heads),]
        arrows(head_from[1], head_from[2], head_to[1], head_to[2],
               col=edgeColor, length=len, lty=lty, lwd=lwd)
    }
    
    drawTxtLabel(txtLabel(x), defaultFontsize=getDefaultAttrs()$edge$fontsize)
})

