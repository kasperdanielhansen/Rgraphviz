##Copyright R. Gentleman 2003, all rights reserved


##some classes for plotting values of nodes and edges

setClass("pNode", representation(name="character",
                                 attrs="list",
                                 subG="integer"),
         prototype=list(subG=as.integer(0)))

setMethod("name", "pNode", function(object)
          object@name)

##end type can be arrow or -| or none
##which can be from, to or both
setClass("pEdge", representation(from="character",
                                 to="character",
                                 attrs="list",
                                 subG="integer"),
         prototype=list(subG=as.integer(0))
)

setGeneric("from", function(object)
           standardGeneric("from"))
setMethod("from", "pEdge", function(object)
          object@from)

setGeneric("to", function(object)
           standardGeneric("to"))
setMethod("to", "pEdge", function(object)
          object@to)


