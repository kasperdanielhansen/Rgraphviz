getDefaultAttrs <- function(layoutType=c("dot","neato","twopi")[1],
                            edgemode=c("undirected","directed")[1]) {
    defAttr <- list(graph=list(), cluster=list(),
                    node=list(), edge=list())

    ## Define the graph attributes
    defAttr$graph$bgcolor <- par("bg")
    defAttr$graph$fontcolor <- par("fg")
    defAttr$graph$ratio <- "fill"
    defAttr$graph$size <- paste(par("pin"),collapse=", ")

    ## Now do layout specific graph attributes
    if (layoutType == "dot") {
        defAttr$graph$rankdir <- "TB"
    }
    else {
        if (layoutType == "neato") {
        }

        ## Not-dot attributes
    }

    ## Now do cluster attributes
    defAttr$cluster$bgcolor <- par("bg")
    defAttr$cluster$color <- par("col")

    ## node attributes
    defAttr$node$shape <- "circle"
    defAttr$node$fixedsize <- TRUE
    defAttr$node$fillcolor <- par("bg")
    defAttr$node$color <- defAttr$node$fillcolor

    ## edge attrs
    defAttr$edge$color <- par("col")
    defAttr$edge$dir <- switch(edgemode,
                               "undirected"="none",
                               "directed"="forward",
                               stop("Invalid edgemode"))

    defAttr
}


checkAttrs <- function(attrs) {
    if (!is.list(attrs))
        stop("attrs must be a list")
    if (length(attrs) != 4)
        stop("attrs must be of length 4")
    if (!all(names(attrs) %in%
             c("graph","cluster", "node","edge")))
        stop(paste("Names of attrs must be 'graph',",
                   "'cluster', 'node', and 'edge'"))
    TRUE
}
