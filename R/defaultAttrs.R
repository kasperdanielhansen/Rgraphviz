getDefaultAttrs <- function(curAttrs=list(),
                            layoutType=c("dot","neato","twopi")[1]) {
    if (length(curAttrs) == 0)
        curAttrs <- list(graph=list(), cluster=list(),
                         node=list(), edge=list())
    else {
        if (is.null(curAttrs$graph)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "graph"
        }
        if (is.null(curAttrs$cluster)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "cluster"
        }
        if (is.null(curAttrs$node)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "node"
        }
        if (is.null(curAttrs$edge)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "edge"
        }
    }

    bg <- "transparent"
    fg <- "black"
    col <- "black"

    ## Define the graph attributes
    if (is.null(curAttrs$graph$bgcolor))
        curAttrs$graph$bgcolor <- bg
    if (is.null(curAttrs$graph$fontcolor))
        curAttrs$graph$fontcolor <- fg
    if (is.null(curAttrs$graph$ratio))
        curAttrs$graph$ratio <- "fill"
    if (is.null(curAttrs$graph$overlap))
        curAttrs$graph$overlap <- ""
    if (is.null(curAttrs$graph$splines))
        curAttrs$graph$splines <- TRUE
    if (is.null(curAttrs$graph$model))
        curAttrs$graph$model <- ""

    ## Now do layout specific graph attributes
    if (layoutType == "dot") {
        if (is.null(curAttrs$graph$rankdir))
            curAttrs$graph$rankdir <- "TB"
    }

    ## Now do cluster attributes
    if (is.null(curAttrs$cluster$bgcolor))
        curAttrs$cluster$bgcolor <- bg
    if (is.null(curAttrs$cluster$color))
        curAttrs$cluster$color <- col

    ## node attributes
    if (is.null(curAttrs$node$shape))
        curAttrs$node$shape <- "circle"
    if (is.null(curAttrs$node$fixedsize))
        curAttrs$node$fixedsize <- TRUE
    if (is.null(curAttrs$node$fillcolor))
        curAttrs$node$fillcolor <- bg
    if (is.null(curAttrs$node$label))
        curAttrs$node$label <- ""
    if (is.null(curAttrs$node$color))
        curAttrs$node$color <- col
    if (is.null(curAttrs$node$fontcolor))
        curAttrs$node$fontcolor <- fg
    if (is.null(curAttrs$node$fontsize))
        curAttrs$node$fontsize <- "14"


    ## edge attrs
    if (is.null(curAttrs$edge$color))
        curAttrs$edge$color <- col
    if (is.null(curAttrs$edge$dir))
        curAttrs$edge$dir <- "both"
    if (is.null(curAttrs$edge$weight))
        curAttrs$edge$weight <- 1.0
    if (is.null(curAttrs$edge$label))
        curAttrs$edge$label <- ""
    if (is.null(curAttrs$edge$fontcolor))
        curAttrs$edge$fontcolor <- fg
    if (is.null(curAttrs$edge$arrowhead))
        curAttrs$edge$arrowhead <- "none"
    if (is.null(curAttrs$edge$arrowtail))
        curAttrs$edge$arrowtail <- "none"
    if (is.null(curAttrs$edge$fontsize))
        curAttrs$edge$fontsize <- "14"
    if (is.null(curAttrs$edge$labelfontsize))
        curAttrs$edge$labelfontsize <- "11"


    curAttrs
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
