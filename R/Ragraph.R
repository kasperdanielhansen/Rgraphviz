.initRagraph <- function(where) {
    setGeneric("Ragraph", function(object)
               standardGeneric("Ragraph"), where=where)

    setClass("Ragraph", representation(agraph="externalptr",
                                       laidout="logical",
                                       nodeLocs="matrix",
                                       numEdges="integer",
                                       edgePoints="list"
                                       )
             )

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

    if (is.null(getGeneric("nodeLocs")))
        setGeneric("nodeLocs", function(object)
                   standardGeneric("nodeLocs"), where=where)
    setMethod("nodeLocs", "Ragraph", function(object)
              object@nodeLocs, where=where)

    if (is.null(getGeneric("numEdges")))
        setGeneric("numEdges", function(object)
                   standardGeneric("numEdges"), where=where)
    setMethod("numEdges", "Ragraph", function(object)
              object@numEdges, where=where)

    if (is.null(getGeneric("edgePoints")))
        setGeneric("edgePoints", function(object)
                   standardGeneric("edgePoints"), where=where)
    setMethod("edgePoints", "Ragraph", function(object)
              object@edgePoints, where=where)

    setMethod("show", "Ragraph", function(object) {
        print(paste("A graph with",nrow(nodeLocs(object)),
                  "nodes."))
    }, where=where)
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
