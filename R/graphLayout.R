graphLayout <- function(graph, layoutType=graph@layoutType, size=NULL)
{
    if (is(graph,"graph"))
        stop("Please use function agopen() for graph objects")

    if (!is(graph,"Ragraph"))
        stop("Object is not of class Ragraph")

    if ( graph@layoutType != layoutType || !graph@laidout || !is.null(size)) {
        graph@layoutType <- layoutType
        z <- .Call("Rgraphviz_doLayout", graph, layoutType, size,
                   PACKAGE="Rgraphviz");
    } else {
        z <- graph
    }
    z
}

buildNodeList <- function(graph, nodeAttrs=list(), subGList=list(),
                          defAttrs=list()) {
    .Call("Rgraphviz_buildNodeList", nodes(graph), nodeAttrs,
                          subGList, defAttrs, PACKAGE="Rgraphviz")
}

buildEdgeList <- function(graph, recipEdges=c("combined", "distinct"),
                          edgeAttrs=list(), subGList=list(), defAttrs=list()) {
    recipEdges <- match.arg(recipEdges)

    edgeNames <- edgeNames(graph, "distinct")

    if ((recipEdges == "combined")&&(length(edgeNames) > 0))
        removed <- which(! edgeNames %in% edgeNames(graph, "combined"))
    else
        removed <- character()

    aa <- edgeL(graph)
    aa <- lapply(aa, function(el) {
        if (length(el) == 0)
            list(edges=numeric())
        else
            el
    })
    
    ## Generate the list of pEdge objects
    .Call("Rgraphviz_buildEdgeList", aa, edgemode(graph),
          subGList, edgeNames, removed, edgeAttrs, defAttrs,
          PACKAGE="Rgraphviz")
}

getNodeNames <- function(object) {
    if (!is(object, "Ragraph")) stop("Need a Ragraph object")
    unlist(lapply(object@AgNode, name))
}

getNodeLabels <- function(object) {
    if (!is(object, "Ragraph")) stop("Need a Ragraph object")
    unlist(lapply(object@AgNode, function(x) labelText(x@txtLabel)))
}

removedEdges <- function(graph) {
    if ((!is(graph, "graph")) && (!is(graph,"Ragraph")))
        stop("removedEdges only accepts objects of class ",
             "'graph' or 'Ragraph'")

    allEdges <- edgeNames(graph, "distinct")
    combEdges <- edgeNames(graph, "combined")
    which(! allEdges %in% combEdges)
}

