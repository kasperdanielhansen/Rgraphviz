.initRagraph <- function(where) {
    setGeneric("Ragraph", function(object)
               standardGeneric("Ragraph"), where=where)

    setClass("Ragraph", representation(agraph="externalptr",
                                       laidout="logical",
                                       nodeLocs="matrix"
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

    setMethod("show", "Ragraph", function(object) {
        cat(paste("A graph with",nrow(nodeLocs(object)),
                  "nodes.\nThis graph has "))
        if (laidout(object))
            cat("already")
        else
            cat("has not")
        cat(" been laid out using GraphViz\n")
    }, where=where)
}

