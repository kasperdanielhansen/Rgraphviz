##Copyright R. Gentleman 2003, all rights reserved


##some classes for plotting values of nodes and edges

setClass("pNode", representation(name="character",
                                 label="character",
                                 shape="character",
                                 color="character"
                                 ),
         )

## Use initialize and not prototype as label's default
## should make it the same as name
setMethod("initialize", "pNode", function(.Object, name = "",
                                          label = name,
                                          shape="circle", color="white") {
    .Object@name <- name
    .Object@label <- label
    .Object@shape <- shape
    .Object@color <- color
    .Object
})

if (is.null(getGeneric("name")))
    setGeneric("name", function(object)
               standardGeneric("name"))
setMethod("name", "pNode", function(object)
          object@name)

if (is.null(getGeneric("label")))
    setGeneric("label", function(object)
               standardGeneric("label"))
setMethod("label", "pNode", function(object)
          object@label)

if (is.null(getGeneric("shape")))
    setGeneric("shape", function(object)
               standardGeneric("shape"))
setMethod("shape", "pNode", function(object)
          object@shape)

if (is.null(getGeneric("color")))
    setGeneric("color", function(object)
               standardGeneric("color"))
setMethod("color", "pNode", function(object)
          object@color)


##end type can be arrow or -| or none
##which can be from, to or both
setClass("pEdge", representation(from="character",
                                 to="character",
                                 style="character",
                                 color="character",
                                 label="character",
                                 arrowhead="character",
                                 dir="character",
                                 fontname="character",
                                 fontcolor="character",
                                 headclip="logical",
                                 headport="character",
                                 tailclip="logical",
                                 weight="numeric"
                                 ),
         prototype=list(label="")
)

if (is.null(getGeneric("label")))
    setGeneric("label", function(object)
               standardGeneric("label"))
setMethod("label", "pEdge", function(object)
          object@label)
