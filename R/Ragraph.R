.initGraphPlotClasses <- function(where) {
    .initRagraph(where)
    .initAgEdge(where)
    .initNodePosition(where)
    .initBezierCurve(where)
    .initBoundingBox(where)
    .initXYPoint(where)
}

.initRagraph <- function(where) {
    setGeneric("Ragraph", function(object)
               standardGeneric("Ragraph"), where=where)

    setClass("Ragraph", representation(agraph="externalptr",
                                       laidout="logical",
                                       nodes="list",
                                       AgEdge="list",
                                       boundBox="boundingBox"
                                       ),
             where=where)

    if (is.null(getGeneric("agraph")))
        setGeneric("agraph", function(object)
                   standardGeneric("agraph"), where=where)
    setMethod("agraph", "Ragraph", function(object)
              object@agraph, where=where)

    if (is.null(getGeneric("laidout")))
        setGeneric("laidout", function(object)
                   standardGeneric("laidout"), where=where)
    setMethod("laidout", "Ragraph", function(object)
              object@laidout, where=where)

    if (is.null(getGeneric("nodes")))
        setGeneric("nodes", function(object)
                   standardGeneric("nodes"), where=where)
    setMethod("nodes", "Ragraph", function(object)
              object@nodes, where=where)

    if (is.null(getGeneric("boundBox")))
        setGeneric("boundBox", function(object)
                   standardGeneric("boundBox"), where=where)
    setMethod("boundBox", "Ragraph", function(object)
              object@boundBox, where=where)

    if (is.null(getGeneric("AgEdge")))
        setGeneric("AgEdge", function(object)
                   standardGeneric("AgEdge"), where=where)
    setMethod("AgEdge", "Ragraph", function(object)
              object@AgEdge, where=where)

    if (is.null(getGeneric("getNodeLocs")))
        setGeneric("getNodeLocs", function(object)
                   standardGeneric("getNodeLocs"), where=where)
    setMethod("getNodeLocs", "Ragraph", function(object) {
        out <- vector(mode="list",length=2)
        names(out) <- c("x","y")
        xys <- lapply(object@nodes,getNodeCenter)
        out[[1]] <- unlist(lapply(xys,getX))
        out[[2]] <- unlist(lapply(xys,getY))
        out
    }, where=where)

    setMethod("show", "Ragraph", function(object) {
        print(paste("A graph with",length(nodes(object)),
                  "nodes."))
    }, where=where)

}

.initBoundingBox <- function(where) {
    if (is.null(getGeneric("boundingBox")))
        setGeneric("boundingBox", function(object)
                   standardGeneric("boundingBox"), where=where)
    setClass("boundingBox", representation(botLeft="xyPoint",
                                           upRight="xyPoint"))

    if (is.null(getGeneric("botLeft")))
        setGeneric("botLeft", function(object)
                   standardGeneric("botLeft"), where=where)
    setMethod("botLeft", "boundingBox", function(object)
              object@botLeft, where=where)

    if (is.null(getGeneric("upRight")))
        setGeneric("upRight", function(object)
                   standardGeneric("upRight"), where=where)
    setMethod("upRight", "boundingBox", function(object)
              object@upRight, where=where)
}

.initNodePosition <- function(where) {
    if (is.null(getGeneric("NodePosition")))
        setGeneric("NodePosition", function(object)
                   standardGeneric("NodePosition"), where=where)
    setClass("NodePosition", representation(center="xyPoint",
                                            height="integer",
                                            rWidth="integer",
                                            lWidth="integer"))
    if (is.null(getGeneric("getNodeCenter")))
        setGeneric("getNodeCenter", function(object)
                   standardGeneric("getNodeCenter"), where=where)
    setMethod("getNodeCenter", "NodePosition", function(object)
              object@center, where=where)

    if (is.null(getGeneric("getNodeHeight")))
        setGeneric("getNodeHeight", function(object)
                   standardGeneric("getNodeHeight"), where=where)
    setMethod("getNodeHeight", "NodePosition", function(object)
              object@height, where=where)

    if (is.null(getGeneric("getNodeRW")))
        setGeneric("getNodeRW", function(object)
                   standardGeneric("getNodeRW"), where=where)
    setMethod("getNodeRW", "NodePosition", function(object)
              object@rWidth, where=where)

    if (is.null(getGeneric("getNodeLW")))
        setGeneric("getNodeLW", function(object)
                  standardGeneric("getNodeLW"), where=where)
    setMethod("getNodeLW", "NodePosition", function(object)
              object@lWidth, where=where)
}

.initAgEdge <- function(where) {
    if (is.null(getGeneric("AgEdge")))
        setGeneric("AgEdge", function(object)
                   standardGeneric("AgEdge"), where=where)
    ## !!! Will want to include edgeID here
    setClass("AgEdge", representation(splines="list",
                                          startArrow="logical",
                                          endArrow="logical",
                                          sp="xyPoint",
                                          ep="xyPoint"))

    if (is.null(getGeneric("splines")))
        setGeneric("splines", function(object)
                   standardGeneric("splines"), where=where)
    setMethod("splines", "AgEdge", function(object)
              object@splines, where=where)

    if (is.null(getGeneric("startArrow")))
        setGeneric("startArrow", function(object)
                  standardGeneric("startArrow"), where=where)
    setMethod("startArrow", "AgEdge", function(object)
              object@startArrow, where=where)
    if (is.null(getGeneric("endArrow")))
        setGeneric("endArrow", function(object)
                  standardGeneric("endArrow"), where=where)
    setMethod("endArrow", "AgEdge", function(object)
              object@endArrow, where=where)

    if (is.null(getGeneric("sp")))
        setGeneric("sp", function(object)
                   standardGeneric("sp"), where=where)
    setMethod("sp", "AgEdge", function(object)
              object@sp, where=where)
    if (is.null(getGeneric("ep")))
        setGeneric("ep", function(object)
                   standardGeneric("ep"), where=where)
    setMethod("ep", "AgEdge", function(object)
              object@ep, where=where)


    if (is.null(getGeneric("numSplines")))
        setGeneric("numSplines", function(object)
                   standardGeneric("numSplines"), where=where)
    setMethod("numSplines", "AgEdge", function(object)
              length(object@splines), where=where)

    if (is.null(getGeneric("getSpline")))
        setGeneric("getSpline", function(object, pos)
                   standardGeneric("getSpline"), where=where)
    setMethod("getSpline", "AgEdge", function(object, pos) {
        if ((pos > 0)&&(pos <= numSplines(object)))
            return(object@splines[[pos]])
        else
            return(NULL)
    }, where=where)

    setMethod("show", "AgEdge", function(object) {
        z <- splines(object)
        out <- paste("An edge with",numSplines(object),"BezierCurve objects:")
        cat(out,"\n")
        for (i in seq(along=z))
            show(z[[i]])
    }, where=where)

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
              }, where=where)
}

.initBezierCurve <- function(where) {
    setGeneric("BezierCurve", function(object)
               standardGeneric("BezierCurve"), where=where)
    setClass("BezierCurve", representation(cPoints="list"))

    if (is.null(getGeneric("cPoints")))
        setGeneric("cPoints", function(object)
                   standardGeneric("cPoints"), where=where)
    setMethod("cPoints", "BezierCurve", function(object)
              object@cPoints, where=where)

    if (is.null(getGeneric("pointList")))
        setGeneric("pointList", function(object)
                   standardGeneric("pointList"), where=where)
    setMethod("pointList", "BezierCurve", function(object) {
        z <- cPoints(object)
        out <- lapply(z, getPoints)
        out
    }, where=where)

    if (is.null(getGeneric("bezierPoints")))
        setGeneric("bezierPoints", function(object)
                   standardGeneric("bezierPoints"), where=where)
    setMethod("bezierPoints", "BezierCurve", function(object) {
        z <- pointList(object)
        out <- vector("list", length=11)
        for (i in 0:10)
            out[[i+1]] <- bezier(z, length(z), i/10)
        out <- matrix(unlist(out), ncol=2, byrow=TRUE,
                      dimnames=list(NULL,c("x","y")))
        out
    }, where=where)

    setMethod("lines", "BezierCurve", function(x,...,col=par("col")) {
        z <- bezierPoints(x)
        lines(z[,1],z[,2],col=col)
    }, where=where)

    setMethod("show", "BezierCurve", function(object) {
        z <- cPoints(object)
        out <- paste(unlist(lapply(z,
                                   function(x){paste(getPoints(x),
                                                     collapse=",")})),
                     collapse=" ")
        out <- paste(out,"\n")
        cat(out)
    }, where=where)
}

.initXYPoint <- function(where) {
    setGeneric("xyPoint", function(object)
               standardGeneric("xyPoint"), where=where)
    setClass("xyPoint", representation(x="numeric",
                                       y="numeric")
             )

    if (is.null(getGeneric("getX")))
        setGeneric("getX", function(object)
                   standardGeneric("getX"), where=where)
    setMethod("getX", "xyPoint", function(object)
              object@x, where=where)

    if (is.null(getGeneric("getY")))
        setGeneric("getY", function(object)
                   standardGeneric("getY"), where=where)
    setMethod("getY", "xyPoint", function(object)
              object@y, where=where)

    if (is.null(getGeneric("getPoints")))
        setGeneric("getPoints", function(object)
                   standardGeneric("getPoints"), where=where)
    setMethod("getPoints", "xyPoint", function(object)
              c(object@x, object@y), where=where)

    setMethod("show", "xyPoint", function(object)
              print(paste(object@x,object@y,sep=",")),
              where=where)
}

bezier <- function(pnts, n, t) {
    ## Used for calculation of bezier splines
    n <- n-1
    x <- 0
    for (k in 0:n) {
        x <- x + (pnts[[k+1]] * choose(n,k) * (t^k) * ((1-t)^(n-k)))
    }
    return(x)
}
