.initRagraph <- function(where) {
    setGeneric("Ragraph", function(object)
               standardGeneric("Ragraph"), where=where)
    ## layout shoudl be changed to a logical
    setClass("Ragraph", representation(agraph="externalptr",
                                       layout="integer"))


    if (is.null(getGeneric("agraph")))
        setGeneric("agraph", function(object)
                   standardGeneric("agraph"), where=where)
    setMethod("agraph", "Ragraph", function(object)
              object@agraph, where=where)

    if (is.null(getGeneric("layout")))
        setGeneric("layout", function(object)
                   standardGeneric("layout"), where=where)
    setMethod("layout", "Ragraph", function(object)
              object@layout, where=where)
}
