require("graph") || stop("Rgraphviz requires package graph")
### CLass xyPoint
setClass("xyPoint", representation(x="numeric",
                                   y="numeric"))

if (is.null(getGeneric("getX")))
    setGeneric("getX", function(object)
               standardGeneric("getX"))
setMethod("getX", "xyPoint", function(object)
          object@x)

if (is.null(getGeneric("getY")))
    setGeneric("getY", function(object)
               standardGeneric("getY"))
setMethod("getY", "xyPoint", function(object)
          object@y)

if (is.null(getGeneric("getPoints")))
    setGeneric("getPoints", function(object)
               standardGeneric("getPoints"))
setMethod("getPoints", "xyPoint", function(object)
          c(object@x, object@y))

### Class AgTextLabel
## used to represent a 'textlabel_t' and related information
setClass("AgTextLabel", representation(labelText="character",
                                       labelLoc="xyPoint",
                                       labelJust="character",
                                       labelWidth="integer",
                                       labelColor="character",
                                       labelFontsize="numeric"))
if (is.null(getGeneric("labelText")))
    setGeneric("labelText", function(object)
               standardGeneric("labelText"))
setMethod("labelText", "AgTextLabel", function(object)
          object@labelText)

if (is.null(getGeneric("labelColor")))
    setGeneric("labelColor", function(object)
               standardGeneric("labelColor"))
setMethod("labelColor", "AgTextLabel", function(object)
          object@labelColor)

if (is.null(getGeneric("labelLoc")))
    setGeneric("labelLoc", function(object)
               standardGeneric("labelLoc"))
setMethod("labelLoc", "AgTextLabel", function(object)
          object@labelLoc)

if (is.null(getGeneric("labelJust")))
    setGeneric("labelJust", function(object)
               standardGeneric("labelJust"))
setMethod("labelJust", "AgTextLabel", function(object)
          object@labelJust)

if (is.null(getGeneric("labelWidth")))
    setGeneric("labelWidth", function(object)
               standardGeneric("labelWidth"))
setMethod("labelWidth","AgTextLabel", function(object)
          object@labelWidth)

if (is.null(getGeneric("labelFontsize")))
    setGeneric("labelFontsize", function(object)
               standardGeneric("labelFontsize"))

setMethod("labelFontsize", "AgTextLabel", function(object)
          object@labelFontsize)


getNodeNames <- function(object) {
    if (!is(object, "Ragraph"))
        stop("Need a Ragraph object")
    unlist(lapply(object@AgNode, name))
}

getNodeLabels <- function(object) {
    if (!is(object, "Ragraph"))
        stop("Need a Ragraph object")
    unlist(lapply(object@AgNode, function(x) labelText(x@txtLabel)))
}

### Class boundingBox

if (is.null(getGeneric("boundingBox")))
    setGeneric("boundingBox", function(object)
               standardGeneric("boundingBox"))
setClass("boundingBox", representation(botLeft="xyPoint",
                                       upRight="xyPoint"))

if (is.null(getGeneric("botLeft")))
    setGeneric("botLeft", function(object)
                   standardGeneric("botLeft"))
setMethod("botLeft", "boundingBox", function(object)
          object@botLeft)

if (is.null(getGeneric("upRight")))
    setGeneric("upRight", function(object)
               standardGeneric("upRight"))
setMethod("upRight", "boundingBox", function(object)
          object@upRight)

### Class AgNode

if (is.null(getGeneric("AgNode")))
    setGeneric("AgNode", function(object)
               standardGeneric("AgNode"))
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

if (is.null(getGeneric("shape")))
    setGeneric("shape", function(object)
               standardGeneric("shape"))
setMethod("shape", "AgNode", function(object)
          object@shape)

if (is.null(getGeneric("style")))
    setGeneric("style", function(object)
               standardGeneric("style"))
setMethod("style", "AgNode", function(object)
          object@style)


if (is.null(getGeneric("color")))
    setGeneric("color", function(object)
               standardGeneric("color"))
setMethod("color","AgNode", function(object)
          object@color)

if (is.null(getGeneric("fillcolor")))
    setGeneric("fillcolor", function(object)
               standardGeneric("fillcolor"))
setMethod("fillcolor", "AgNode", function(object)
          object@fillcolor)

if (is.null(getGeneric("getNodeCenter")))
    setGeneric("getNodeCenter", function(object)
               standardGeneric("getNodeCenter"))
setMethod("getNodeCenter", "AgNode", function(object)
          object@center)

if (is.null(getGeneric("getNodeHeight")))
    setGeneric("getNodeHeight", function(object)
               standardGeneric("getNodeHeight"))
setMethod("getNodeHeight", "AgNode", function(object)
          object@height)

if (is.null(getGeneric("getNodeRW")))
    setGeneric("getNodeRW", function(object)
               standardGeneric("getNodeRW"))
setMethod("getNodeRW", "AgNode", function(object)
          object@rWidth)


if (is.null(getGeneric("getNodeLW")))
    setGeneric("getNodeLW", function(object)
               standardGeneric("getNodeLW"))
setMethod("getNodeLW", "AgNode", function(object)
          object@lWidth)



if (is.null(getGeneric("name")))
    setGeneric("name", function(object)
               standardGeneric("name"))
setMethod("name", "AgNode", function(object)
          object@name)

if (is.null(getGeneric("txtLabel")))
    setGeneric("txtLabel", function(object)
               standardGeneric("txtLabel"))
setMethod("txtLabel", "AgNode", function(object)
          object@txtLabel)

if (is.null(getGeneric("getNodeXY")))
    setGeneric("getNodeXY", function(object)
               standardGeneric("getNodeXY"))


setMethod("getNodeXY", "AgNode", function(object) {
    cen <- getNodeCenter(object)
    out <- list(x=getX(cen), y=getY(cen))
    out
})

### Class AgEdge

if (is.null(getGeneric("AgEdge")))
    setGeneric("AgEdge", function(object)
               standardGeneric("AgEdge"))

setClass("AgEdge", representation(splines="list",
                                  sp="xyPoint",
                                  ep="xyPoint",
                                  head="character",
                                  tail="character",
                                  arrowhead="character",
                                  arrowtail="character",
                                  arrowsize="character",
                                  color="character",
                                  txtLabel="AgTextLabel"))

if (is.null(getGeneric("color")))
    setGeneric("color", function(object)
               standardGeneric("color"))
setMethod("color","AgEdge", function(object)
          object@color)

if (is.null(getGeneric("arrowsize")))
    setGeneric("arrowsize", function(object)
               standardGeneric("arrowsize"))
setMethod("arrowsize", "AgEdge", function(object)
          object@arrowsize)

if (is.null(getGeneric("arrowhead")))
    setGeneric("arrowhead", function(object)
               standardGeneric("arrowhead"))
setMethod("arrowhead", "AgEdge", function(object)
          object@arrowhead)

if (is.null(getGeneric("arrowtail")))
    setGeneric("arrowtail", function(object)
               standardGeneric("arrowtail"))
setMethod("arrowtail", "AgEdge", function(object)
          object@arrowtail)

if (is.null(getGeneric("txtLabel")))
    setGeneric("txtLabel", function(object)
               standardGeneric("txtLabel"))
setMethod("txtLabel", "AgEdge", function(object)
          object@txtLabel)

if (is.null(getGeneric("splines")))
    setGeneric("splines", function(object)
               standardGeneric("splines"))
setMethod("splines", "AgEdge", function(object)
          object@splines)

if (is.null(getGeneric("sp")))
    setGeneric("sp", function(object)
               standardGeneric("sp"))
setMethod("sp", "AgEdge", function(object)
          object@sp)
if (is.null(getGeneric("ep")))
    setGeneric("ep", function(object)
               standardGeneric("ep"))
setMethod("ep", "AgEdge", function(object)
          object@ep)

if (is.null(getGeneric("head")))
    setGeneric("head", function(x,...)
               standardGeneric("head"))
setMethod("head", "AgEdge", function(x, ...)
          x@head)

if (is.null(getGeneric("tail")))
    setGeneric("tail", function(x, ...)
               standardGeneric("tail"))
setMethod("tail", "AgEdge", function(x, ...)
          x@tail)

if (is.null(getGeneric("numSplines")))
    setGeneric("numSplines", function(object)
               standardGeneric("numSplines"))
setMethod("numSplines", "AgEdge", function(object)
          length(object@splines))

if (is.null(getGeneric("getSpline")))
    setGeneric("getSpline", function(object, pos)
               standardGeneric("getSpline"))
setMethod("getSpline", "AgEdge", function(object, pos) {
    if ((pos > 0)&&(pos <= numSplines(object)))
        return(object@splines[[pos]])
    else
        return(NULL)

})

### Class BezierCurve

setClass("BezierCurve", representation(cPoints="list"))

if (is.null(getGeneric("cPoints")))
    setGeneric("cPoints", function(object)
               standardGeneric("cPoints"))
setMethod("cPoints", "BezierCurve", function(object)
          object@cPoints)

if (is.null(getGeneric("pointList")))
    setGeneric("pointList", function(object)
               standardGeneric("pointList"))
setMethod("pointList", "BezierCurve", function(object) {
    z <- cPoints(object)
    out <- lapply(z, getPoints)
    out
})

if (is.null(getGeneric("bezierPoints")))
    setGeneric("bezierPoints", function(object)
               standardGeneric("bezierPoints"))
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
    if (is.null(getGeneric("bLines")))
        setGeneric("bLines", function(x, ...)
                   standardGeneric("bLines"))
setMethod("bLines", "BezierCurve", function(x,...,col=par("col"),
                                                len=0.25, lty=par("lty"),
                                                lwd=par("lwd"),
                                                arrowtail="none",
                                                arrowhead="none") {
    z <- bezierPoints(x)

    numSegs <- nrow(z)
    lines(z[2:(numSegs-1),1], z[2:(numSegs-1),2],
          col=col, lty=lty, lwd=lwd)

    tailStart <- z[2,]
    tailEnd <- z[1,]
    switch(arrowtail,
           "none"=lines(c(tailStart[1], tailEnd[1]),
                            c(tailStart[2], tailEnd[2]), col=col, lty=lty,
           lwd=lwd),
           "open"=arrows(tailStart[1], tailStart[2], tailEnd[1],
                             tailEnd[2], col=col, length=len, lty=lty,
           lwd=lwd),
           stop("Unsupported arrowtail type: ", arrowtail))

    headStart <- z[numSegs-1,]
    headEnd <- z[numSegs,]

    switch(arrowhead,
           "none"=lines(c(headStart[1], headEnd[1]),
           c(headStart[2], headEnd[2]), col=col, lty=lty,
           lwd=lwd),
           "open"=arrows(headStart[1], headStart[2], headEnd[1],
           headEnd[2], col=col, length=len, lty=lty,
           lwd=lwd),
           stop("Unsupported arrowhead type: ", arrowhead))

})





setMethod("show", "xyPoint", function(object)
          cat(paste("x: ", object@x, ", y: ",
                    object@y, "\n", sep="")))

setMethod("show", "AgEdge", function(object) {
    z <- splines(object)
    out <- paste("An edge between", head(object),
                 "and", tail(object),
                 "with",numSplines(object),"BezierCurve objects:")
    cat(out,"\n")
    for (i in seq(along=z))
        show(z[[i]])
})

setMethod("show", "BezierCurve", function(object) {
    z <- cPoints(object)
    out <- paste(unlist(lapply(z,
                               function(x){paste(getPoints(x),
                                                 collapse=",")})),
                 collapse=" ")
    out <- paste(out,"\n")
    cat(out)
})

setMethod("lines", "BezierCurve", function(x,...,col=par("col"),
                                           lty=par("lty"), lwd=par("lwd")) {
    z <- bezierPoints(x)
    lines(z[,1],z[,2],col=col,lty=lty,lwd=lwd)
})

setMethod("lines", "AgEdge",
  function(x, ...,
           len,
           lty=par("lty"), lwd=par("lwd")) {
    z <- splines(x)

    arrowtails <- c(arrowtail(x), rep("none", length(z)-1))
    arrowheads <- c(rep("none", length(z)-1), arrowhead(x))

    len <- len * as.numeric(arrowsize(x))
    mapply(bLines, z, arrowhead=arrowheads, arrowtail=arrowtails,
           MoreArgs=list(len=len, col=color(x),
                         lty=lty, lwd=lwd, ...))

    drawTxtLabel(txtLabel(x))
  })

### Class Ragraph
setGeneric("Ragraph", function(object)
           standardGeneric("Ragraph"))

setClass("Ragraph", representation(agraph="externalptr",
                                   laidout="logical",
                                   layoutType="character",
                                   edgemode="character",
                                   AgNode="list",
                                   AgEdge="list",
                                   boundBox="boundingBox"))

if (is.null(getGeneric("agraph")))
    setGeneric("agraph", function(object)
               standardGeneric("agraph"))
setMethod("agraph", "Ragraph", function(object)
          object@agraph)

if (is.null(getGeneric("edgemode")))
    setGeneric("edgemode", function(object)
               standardGeneric("edgemode"))
setMethod("edgemode", "Ragraph", function(object)
          object@edgemode)

if (is.null(getGeneric("laidout")))
    setGeneric("laidout", function(object)
               standardGeneric("laidout"))
setMethod("laidout", "Ragraph", function(object)
          object@laidout)

if (is.null(getGeneric("layoutType")))
    setGeneric("layoutType", function(object)
               standardGeneric("layoutType"))
setMethod("layoutType", "Ragraph", function(object)
          object@layoutType)

if (is.null(getGeneric("boundBox")))
    setGeneric("boundBox", function(object)
               standardGeneric("boundBox"))
setMethod("boundBox", "Ragraph", function(object)
              object@boundBox)

if (is.null(getGeneric("AgEdge")))
    setGeneric("AgEdge", function(object)
               standardGeneric("AgEdge"))
setMethod("AgEdge", "Ragraph", function(object)
          object@AgEdge)

if (is.null(getGeneric("AgNode")))
    setGeneric("AgNode", function(object)
               standardGeneric("AgNode"))
setMethod("AgNode", "Ragraph", function(object)
          object@AgNode)


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
    print(paste("A graph with",length(AgNode(object)),
                "nodes."))
})
