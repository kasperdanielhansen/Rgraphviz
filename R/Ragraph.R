require(graph) || stop("Rgraphviz requires package graph")

### Class Ragraph
setGeneric("Ragraph", function(object)
           standardGeneric("Ragraph"))

setClass("Ragraph", representation(agraph="externalptr",
                                   laidout="logical",
                                   nodes="list",
                                   AgEdge="list",
                                   boundBox="boundingBox"
                                   ))

    if (is.null(getGeneric("agraph")))
    setGeneric("agraph", function(object)
               standardGeneric("agraph"))
setMethod("agraph", "Ragraph", function(object)
          object@agraph)

if (is.null(getGeneric("laidout")))
    setGeneric("laidout", function(object)
               standardGeneric("laidout"))
setMethod("laidout", "Ragraph", function(object)
          object@laidout)

if (is.null(getGeneric("nodes")))
    setGeneric("nodes", function(object)
               standardGeneric("nodes"))
setMethod("nodes", "Ragraph", function(object)
          object@nodes)

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

if (is.null(getGeneric("getNodeLocs")))
        setGeneric("getNodeLocs", function(object)
                   standardGeneric("getNodeLocs"))
setMethod("getNodeLocs", "Ragraph", function(object) {
    out <- vector(mode="list",length=2)
    names(out) <- c("x","y")
    xys <- lapply(object@nodes,getNodeCenter)
    out[[1]] <- unlist(lapply(xys,getX))
    out[[2]] <- unlist(lapply(xys,getY))
    out
})

setMethod("show", "Ragraph", function(object) {
    print(paste("A graph with",length(nodes(object)),
                "nodes."))
})

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

### Class NodePosition

if (is.null(getGeneric("NodePosition")))
    setGeneric("NodePosition", function(object)
               standardGeneric("NodePosition"))
setClass("NodePosition", representation(center="xyPoint",
                                        height="integer",
                                        rWidth="integer",
                                        lWidth="integer"))
if (is.null(getGeneric("getNodeCenter")))
    setGeneric("getNodeCenter", function(object)
               standardGeneric("getNodeCenter"))
setMethod("getNodeCenter", "NodePosition", function(object)
          object@center)

if (is.null(getGeneric("getNodeHeight")))
    setGeneric("getNodeHeight", function(object)
               standardGeneric("getNodeHeight"))
setMethod("getNodeHeight", "NodePosition", function(object)
          object@height)

if (is.null(getGeneric("getNodeRW")))
    setGeneric("getNodeRW", function(object)
               standardGeneric("getNodeRW"))
setMethod("getNodeRW", "NodePosition", function(object)
          object@rWidth)

if (is.null(getGeneric("getNodeLW")))
    setGeneric("getNodeLW", function(object)
               standardGeneric("getNodeLW"))
setMethod("getNodeLW", "NodePosition", function(object)
          object@lWidth)


### Class AgEdge

if (is.null(getGeneric("AgEdge")))
    setGeneric("AgEdge", function(object)
               standardGeneric("AgEdge"))
## !!! Will want to include edgeID here
setClass("AgEdge", representation(splines="list",
                                  startArrow="logical",
                                  endArrow="logical",
                                  sp="xyPoint",
                                  ep="xyPoint",
                                  head="character",
                                  tail="character"))

if (is.null(getGeneric("splines")))
    setGeneric("splines", function(object)
               standardGeneric("splines"))
setMethod("splines", "AgEdge", function(object)
          object@splines)

if (is.null(getGeneric("startArrow")))
    setGeneric("startArrow", function(object)
               standardGeneric("startArrow"))
setMethod("startArrow", "AgEdge", function(object)
          object@startArrow)
if (is.null(getGeneric("endArrow")))
    setGeneric("endArrow", function(object)
               standardGeneric("endArrow"))
setMethod("endArrow", "AgEdge", function(object)
          object@endArrow)

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
    setGeneric("head", function(object,...)
               standardGeneric("head"))
setMethod("head", "AgEdge", function(object, ...)
          object@head)

if (is.null(getGeneric("tail")))
    setGeneric("tail", function(object, ...)
               standardGeneric("tail"))
setMethod("tail", "AgEdge", function(object, ...)
          object@tail)

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

setMethod("show", "AgEdge", function(object) {
    z <- splines(object)
    out <- paste("An edge with",numSplines(object),"BezierCurve objects:")
    cat(out,"\n")
    for (i in seq(along=z))
        show(z[[i]])
})

setMethod("lines","AgEdge",
          function(x,...,col=par("col"),len=0.25) {
              z <- splines(x)
              lapply(z,lines,col=col)

              ## Now need to draw the appropriate arrows, if any
              if (startArrow(x)) {
                  ## Draw end arrow
                  ## get the first point of the first splie
                  curP <-cPoints(z[[1]])[[1]]
                  ## get the edge's ep
                  curSP <- sp(x)
                  arrows(getX(curP), getY(curP), getX(curSP),
                         getY(curSP), col=col, length=len)
              }
              if (endArrow(x)) {
                  ## Draw start arrow
                  ## get the last point of the last spline
                  epPoints <- cPoints(z[[length(z)]])
                  curP <- epPoints[[length(epPoints)]]
                  ## get the edge's sp
                  curEP <- ep(x)
                  arrows(getX(curP), getY(curP), getX(curEP),
                         getY(curEP), col=col, length=len)
              }

              return(NULL)
          })

### Class BezierCurve

setGeneric("BezierCurve", function(object)
           standardGeneric("BezierCurve"))
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
        out[[i+1]] <- bezier(z, length(z), i/10)
    out <- matrix(unlist(out), ncol=2, byrow=TRUE,
                  dimnames=list(NULL,c("x","y")))
    out
})

setMethod("lines", "BezierCurve", function(x,...,col=par("col")) {
    z <- bezierPoints(x)
    lines(z[,1],z[,2],col=col)
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


### CLass xyPoint
setGeneric("xyPoint", function(object)
           standardGeneric("xyPoint"))
setClass("xyPoint", representation(x="numeric",
                                   y="numeric")
         )

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

setMethod("show", "xyPoint", function(object)
          print(paste(object@x,object@y,sep=",")))

bezier <- function(pnts, n, t) {
    ## Used for calculation of bezier splines
    n <- n-1
    x <- 0
    for (k in 0:n) {
        x <- x + (pnts[[k+1]] * choose(n,k) * (t^k) * ((1-t)^(n-k)))
    }
    return(x)
}


