.initGraphPlotClasses <- function(where) {
    .initRagraph(where)
    .initEdgePoints(where)
    .initNodeLayout(where)
    .initControlPoints(where)
    .initBoundingBox(where)
    .initXYPoint(where)

    .initRgraphvizPlot(where)
}

.initRgraphvizPlot <- function(where) {
    setClass("RgraphvizPlot", representation(), where=where)


}

.initRagraph <- function(where) {
    setGeneric("Ragraph", function(object)
               standardGeneric("Ragraph"), where=where)

    setClass("Ragraph", representation(agraph="externalptr",
                                       laidout="logical",
                                       nodes="list",
                                       edgePoints="list",
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

    if (is.null(getGeneric("edgePoints")))
        setGeneric("edgePoints", function(object)
                   standardGeneric("edgePoints"), where=where)
    setMethod("edgePoints", "Ragraph", function(object)
              object@edgePoints, where=where)

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

.initNodeLayout <- function(where) {
    if (is.null(getGeneric("nodeLayout")))
        setGeneric("nodeLayout", function(object)
                   standardGeneric("nodeLayout"), where=where)
    setClass("nodeLayout", representation(center="xyPoint",
                                          height="integer",
                                          rWidth="integer",
                                          lWidth="integer"))
    if (is.null(getGeneric("getNodeCenter")))
        setGeneric("getNodeCenter", function(object)
                   standardGeneric("getNodeCenter"), where=where)
    setMethod("getNodeCenter", "nodeLayout", function(object)
              object@center, where=where)

    if (is.null(getGeneric("getNodeHeight")))
        setGeneric("getNodeHeight", function(object)
                   standardGeneric("getNodeHeight"), where=where)
    setMethod("getNodeHeight", "nodeLayout", function(object)
              object@height, where=where)

    if (is.null(getGeneric("getNodeRW")))
        setGeneric("getNodeRW", function(object)
                   standardGeneric("getNodeRW"), where=where)
    setMethod("getNodeRW", "nodeLayout", function(object)
              object@rWidth, where=where)

    if (is.null(getGeneric("getNodeLW")))
        setGeneric("getNodeLW", function(object)
                  standardGeneric("getNodeLW"), where=where)
    setMethod("getNodeLW", "nodeLayout", function(object)
              object@lWidth, where=where)

}

.initEdgePoints <- function(where) {
    if (is.null(getGeneric("edgePoints")))
        setGeneric("edgePoints", function(object)
                   standardGeneric("edgePoints"), where=where)
    ## !!! Will want to include edgeID here
    setClass("edgePoints", representation(splines="list"))

    if (is.null(getGeneric("splines")))
        setGeneric("splines", function(object)
                   standardGeneric("splines"), where=where)
    setMethod("splines", "edgePoints", function(object)
              object@splines, where=where)

    if (is.null(getGeneric("numSplines")))
        setGeneric("numSplines", function(object)
                   standardGeneric("numSplines"), where=where)
    setMethod("numSplines", "edgePoints", function(object)
              length(object@splines), where=where)

    if (is.null(getGeneric("getSpline")))
        setGeneric("getSpline", function(object, pos)
                   standardGeneric("getSpline"), where=where)
    setMethod("getSpline", "edgePoints", function(object, pos) {
        if ((pos > 0)&&(pos <= numSplines(object)))
            return(object@splines[[pos]])
        else
            return(NULL)
    }, where=where)

    setMethod("show", "edgePoints", function(object) {
        z <- splines(object)
        for (i in seq(along=z))
            show(z[[i]])
    }, where=where)

    setMethod("plot",c("edgePoints","missing"),
              function(x,y,...) {
                  z <- splines(x)
                  lapply(z,function(a){q <- bezierPoints(a);
                                       lines(q[,1],q[,2])})
                  return(NULL)
              }, where=where)
}

.initControlPoints <- function(where) {
    setGeneric("controlPoints", function(object)
               standardGeneric("controlPoints"), where=where)
    setClass("controlPoints", representation(cPoints="list"))

    if (is.null(getGeneric("cPoints")))
        setGeneric("cPoints", function(object)
                   standardGeneric("cPoints"), where=where)
    setMethod("cPoints", "controlPoints", function(object)
              object@cPoints, where=where)

    if (is.null(getGeneric("pointList")))
        setGeneric("pointList", function(object)
                   standardGeneric("pointList"), where=where)
    setMethod("pointList", "controlPoints", function(object) {
        z <- cPoints(object)
        out <- lapply(z, getPoints)
        out
    }, where=where)

    if (is.null(getGeneric("bezierPoints")))
        setGeneric("bezierPoints", function(object)
                   standardGeneric("bezierPoints"), where=where)
    setMethod("bezierPoints", "controlPoints", function(object) {
        z <- pointList(object)
        out <- vector("list", length=11)
        for (i in 0:10)
            out[[i+1]] <- bezier(z, length(z), i/10)
        out <- matrix(unlist(out), ncol=2, byrow=TRUE,
                      dimnames=list(NULL,c("x","y")))
        out
    }, where=where)

    setMethod("show", "controlPoints", function(object) {
        z <- cPoints(object)
        out <- paste(unlist(lapply(z,
                                   function(x){paste(getPoints(x),
                                                     collapse=",")})),
                     collapse=" ")
        print(out)
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
