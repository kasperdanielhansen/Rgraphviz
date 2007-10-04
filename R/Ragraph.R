### CLass xyPoint
setClass("xyPoint", representation(x="numeric", y="numeric"))

setGeneric("getX", function(object) standardGeneric("getX"))
setMethod("getX", "xyPoint", function(object) object@x)

setGeneric("getY", function(object) standardGeneric("getY"))
setMethod("getY", "xyPoint", function(object) object@y)

setGeneric("getPoints", function(object) standardGeneric("getPoints"))
setMethod("getPoints", "xyPoint", function(object) c(object@x, object@y))


### Class AgTextLabel
## used to represent a 'textlabel_t' and related information
setClass("AgTextLabel", representation(labelText="character",
                                       labelLoc="xyPoint",
                                       labelJust="character",
                                       labelWidth="integer",
                                       labelColor="character",
                                       labelFontsize="numeric"))
setGeneric("labelText", function(object) standardGeneric("labelText"))
setMethod("labelText", "AgTextLabel", function(object) object@labelText)

setGeneric("labelColor", function(object) standardGeneric("labelColor"))
setMethod("labelColor", "AgTextLabel", function(object) object@labelColor)

setGeneric("labelLoc", function(object) standardGeneric("labelLoc"))
setMethod("labelLoc", "AgTextLabel", function(object) object@labelLoc)

setGeneric("labelJust", function(object) standardGeneric("labelJust"))
setMethod("labelJust", "AgTextLabel", function(object) object@labelJust)

setGeneric("labelWidth", function(object) standardGeneric("labelWidth"))
setMethod("labelWidth","AgTextLabel", function(object) object@labelWidth)

setGeneric("labelFontsize", function(object) standardGeneric("labelFontsize"))
setMethod("labelFontsize", "AgTextLabel", function(object) object@labelFontsize)


### Class boundingBox
setClass("boundingBox", representation(botLeft="xyPoint", upRight="xyPoint"))

setGeneric("botLeft", function(object) standardGeneric("botLeft"))
setMethod("botLeft", "boundingBox", function(object) object@botLeft)

setGeneric("upRight", function(object) standardGeneric("upRight"))
setMethod("upRight", "boundingBox", function(object) object@upRight)


### Class AgNode
setClass("AgNode", representation(center="xyPoint",
                                  name="character",
                                  txtLabel="AgTextLabel",
                                  height="integer",
                                  rWidth="integer",
                                  lWidth="integer",
                                  color="character",
                                  fillcolor="character",
                                  shape="character",
                                  style="character"))

setGeneric("shape", function(object) standardGeneric("shape"))
setMethod("shape", "AgNode", function(object) object@shape)

setGeneric("style", function(object) standardGeneric("style"))
setMethod("style", "AgNode", function(object) object@style)

setGeneric("color", function(object) standardGeneric("color"))
setMethod("color","AgNode", function(object) object@color)

setGeneric("fillcolor", function(object) standardGeneric("fillcolor"))
setMethod("fillcolor", "AgNode", function(object) object@fillcolor)

setGeneric("getNodeCenter", function(object) standardGeneric("getNodeCenter"))
setMethod("getNodeCenter", "AgNode", function(object) object@center)

setGeneric("getNodeHeight", function(object) standardGeneric("getNodeHeight"))
setMethod("getNodeHeight", "AgNode", function(object) object@height)

setGeneric("getNodeRW", function(object) standardGeneric("getNodeRW"))
setMethod("getNodeRW", "AgNode", function(object) object@rWidth)

setGeneric("getNodeLW", function(object) standardGeneric("getNodeLW"))
setMethod("getNodeLW", "AgNode", function(object) object@lWidth)

setGeneric("name", function(object) standardGeneric("name"))
setMethod("name", "AgNode", function(object) object@name)

setGeneric("txtLabel", function(object) standardGeneric("txtLabel"))
setMethod("txtLabel", "AgNode", function(object) object@txtLabel)

setGeneric("getNodeXY", function(object) standardGeneric("getNodeXY"))
setMethod("getNodeXY", "AgNode", function(object) {
    cen <- getNodeCenter(object)
    out <- list(x=getX(cen), y=getY(cen))
    out
})


### Class AgEdge
setClass("AgEdge", representation(splines="list",
                                  sp="xyPoint",
                                  ep="xyPoint",
                                  head="character",
                                  tail="character",
			          dir="character",
                                  arrowhead="character",
                                  arrowtail="character",
                                  arrowsize="character",
                                  color="character",
                                  lty="character",
                                  lwd="numeric",
                                  txtLabel="AgTextLabel"))

setMethod("color","AgEdge", function(object) object@color)

setGeneric("arrowsize", function(object) standardGeneric("arrowsize"))
setMethod("arrowsize", "AgEdge", function(object) object@arrowsize)

setGeneric("arrowhead", function(object) standardGeneric("arrowhead"))
setMethod("arrowhead", "AgEdge", function(object) object@arrowhead)

setGeneric("arrowtail", function(object) standardGeneric("arrowtail"))
setMethod("arrowtail", "AgEdge", function(object) object@arrowtail)

setMethod("txtLabel", "AgEdge", function(object) object@txtLabel)

setGeneric("splines", function(object) standardGeneric("splines"))
setMethod("splines", "AgEdge", function(object) object@splines)

setGeneric("sp", function(object) standardGeneric("sp"))
setMethod("sp", "AgEdge", function(object) object@sp)

setGeneric("ep", function(object) standardGeneric("ep"))
setMethod("ep", "AgEdge", function(object) object@ep)

### Don't make "head" or "tail" generics here: they are already defined as
### generics in package utils.
setMethod("head", "AgEdge", function(x, ...) x@head)
setMethod("tail", "AgEdge", function(x, ...) x@tail)

setGeneric("numSplines", function(object) standardGeneric("numSplines"))
setMethod("numSplines", "AgEdge", function(object) length(object@splines))

setGeneric("getSpline", function(object, pos) standardGeneric("getSpline"))
setMethod("getSpline", "AgEdge", function(object, pos) {
    if ((pos > 0)&&(pos <= numSplines(object)))
        return(object@splines[[pos]])
    else
        return(NULL)

})


### Class BezierCurve
setClass("BezierCurve", representation(cPoints="list"))

setGeneric("cPoints", function(object) standardGeneric("cPoints"))
setMethod("cPoints", "BezierCurve", function(object) object@cPoints)

setGeneric("pointList", function(object) standardGeneric("pointList"))
setMethod("pointList", "BezierCurve", function(object) {
    z <- cPoints(object)
    out <- lapply(z, getPoints)
    out
})

setGeneric("bezierPoints", function(object) standardGeneric("bezierPoints"))
setMethod("bezierPoints", "BezierCurve", function(object) {
    z <- pointList(object)
    out <- vector("list", length=11)
    for (i in 0:10)
        out[[i+1]] <- .Call("Rgraphviz_bezier", z, length(z), i/10,
                            PACKAGE="Rgraphviz")
    out <- matrix(unlist(out), ncol=2, byrow=TRUE,
                  dimnames=list(NULL,c("x","y")))
    out
})

## TODO: this should be obsolete
setGeneric("bLines", function(x, ...) standardGeneric("bLines"))
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


## show
setMethod("show", "xyPoint", function(object)
          cat(paste("x: ", object@x, ", y: ", object@y, "\n", sep="")))

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

setMethod("show", "BezierCurve", function(object) {
    z <- cPoints(object)
    out <- paste(unlist(lapply(z,
                 function(x){paste(getPoints(x), collapse=",")})), collapse=" ")
    out <- paste(out,"\n")
    cat(out)
})


## lines
##FIXME: do we need this anymore?
setMethod("lines", "BezierCurve", 
		function(x,...,col=par("col"), lty=par("lty"), lwd=par("lwd")) {
    z <- bezierPoints(x)
    lines(z[,1],z[,2],col=col,lty=lty,lwd=lwd)
})

setMethod("lines", "AgEdge",
  		function(x, ..., len, lty=par("lty"), lwd=par("lwd")) {
    z <- splines(x)

    edgeColor <- color(x)
    if (edgeColor == "") edgeColor <- "black"

    arrowSize <- arrowsize(x)
    if ( arrowSize == "" ) arrowSize = "1"

    if(length(x@lty)>0) lty=x@lty[1]
    if(length(x@lwd)>0) lwd=x@lwd[1]

    len <- len * as.numeric(arrowSize)
    mapply(lines, z, MoreArgs=list(len=len, col=edgeColor, lty=lty, lwd=lwd, ...))

    # TODO: arrow shapes should/could be from arrowtail/head 
    if ( x@dir == "both" || x@dir== "back" )
    {
       tails = bezierPoints(z[[1]])
       tail_from = tails[2, ]
       tail_to   = tails[1, ]
       arrows(tail_from[1], tail_from[2], tail_to[1], tail_to[2],
			col=edgeColor, length=len, lty=lty, lwd=lwd)
    }
    if ( x@dir == "both" || x@dir == "forward" )
    {
       heads = bezierPoints(z[[length(z)]])
       head_from = heads[nrow(heads)-1, ]
       head_to   = heads[nrow(heads),]
       arrows(head_from[1], head_from[2], head_to[1], head_to[2],
			col=edgeColor, length=len, lty=lty, lwd=lwd)
    }

    drawTxtLabel(txtLabel(x))
})

### Class Ragraph
setClass("Ragraph", representation(agraph="externalptr",
                                   laidout="logical",
                                   layoutType="character",
                                   edgemode="character",
                                   AgNode="list",
                                   AgEdge="list",
                                   boundBox="boundingBox",
				   fg="character",
				   bg="character"))

setGeneric("agraph", function(object) standardGeneric("agraph"))
setMethod("agraph", "Ragraph", function(object) object@agraph)

setMethod("edgemode", "Ragraph", function(object) object@edgemode)

setGeneric("laidout", function(object) standardGeneric("laidout"))
setMethod("laidout", "Ragraph", function(object) object@laidout)

setGeneric("layoutType", function(object) standardGeneric("layoutType"))
setMethod("layoutType", "Ragraph", function(object) object@layoutType)

setGeneric("boundBox", function(object) standardGeneric("boundBox"))
setMethod("boundBox", "Ragraph", function(object) object@boundBox)


##------------------------------------------------------------
## accessor and replacement methods for AgEdge slot
##------------------------------------------------------------
setGeneric("AgEdge", function(object) standardGeneric("AgEdge"))
setMethod("AgEdge", "Ragraph", function(object) object@AgEdge)

setGeneric("AgEdge<-", function(object, value) standardGeneric("AgEdge<-"))
setReplaceMethod("AgEdge", "Ragraph", function(object, value) {
                   object@AgEdge = value
                   return(object)
})

##------------------------------------------------------------
## accessor and replacement methods for AgNode slot
##------------------------------------------------------------
setGeneric("AgNode", function(object) standardGeneric("AgNode"))
setMethod("AgNode", "Ragraph", function(object) object@AgNode)

setGeneric("AgNode<-", function(object, value) standardGeneric("AgNode<-"))
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


setMethod("edgeNames", "Ragraph", 
                    function(object, recipEdges=c("combined", "distinct")) {
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

