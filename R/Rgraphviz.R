agopen <- function(graph, name, kind=NULL, layout=TRUE,
                   layoutType=c("dot","neato","twopi")[1],
                   attrs=getDefaultAttrs(layoutType),
                   nodeAttrs=list(), edgeAttrs=list(),
                   subGList=list()) {

      checkAttrs(attrs)

      nodes <- buildNodeList(graph, nodeAttrs, subGList)
      edges <- buildEdgeList(graph, edgeAttrs, subGList)

      if (length(subGList) > 0)
          subGs <- paste("cluster_",1:length(subGList), sep="")
      else
          subGs <- character()

      if (is.null(kind)) {
          ## Determine kind from the graph object
          outK <- switch(edgemode(graph),
                         "undirected"=0,  ## AGRAPH
                         "directed"=1,    ## AGDIGRAPH
                         0)
      }
      else {
          ## Use the specified 'kind' parameter.
          outK <- switch(kind,
                         "AGRAPH"=0,   ##Undirected Graph
                         "AGDIGRAPH"=1,   ## directed graph
                         "AGRAPHSTRICT"=2,   ## no self arcs or multiedges
                         "AGDIGRAPHSTRICT"=3, ## directed strict graph
                         stop(paste("Incorrect kind parameter:",kind)))
      }

      ## all attrs must be character strings going into C,
      ## graphviz wants all attrs to be char*
      ## FIXME: so shouldn't we do that in C?
      attrs <- lapply(attrs, function(x){lapply(x,as.character)})

      g <- .Call("Rgraphviz_agopen", as.character(name),
                 as.integer(outK), as.list(nodes),
                 as.list(edges), as.list(attrs),
                 as.character(subGs))
      g@layoutType <- layoutType
      g@edgemode <- edgemode(graph)
      g@nodes <- nodes
      g@edges <- edges

      if (layout)
          return(layoutGraph(g))
      else
          return(g)
  }


agread <- function(filename, layoutType=c("dot","neato","twopi")[1],
                   layout=TRUE) {
    ## First check that the file exists
    if (!file.exists(filename))
        stop(paste("Request file",filename,"does not exist"))

    g <- .Call("Rgraphviz_agread", as.character(filename))

    if (layout)
        return(layoutGraph(g,layoutType))
    else
        return(g)
}

agwrite <- function(graph, filename) {
    g <- .Call("Rgraphviz_agwrite", graph, as.character(filename))
}


layoutGraph <- function(graph) {
    if (inherits(graph,"graphNEL"))
        stop("Please use function agopen() for graphNEL objects")
    if (!inherits(graph,"Ragraph"))
        stop("Object is not of class Ragraph")

    type <- switch(layoutType(graph),
                   "dot"=0,
                   "neato"=1,
                   "twopi"=2,
                   stop(paste("Invalid layout type:",layoutType))
                   )

    if (laidout(graph) == FALSE) {
        z <- .Call("Rgraphviz_doLayout", graph, as.integer(type));
        return(z)
    }
    else {
        return(graph)
    }
}

graphvizVersion <- function() {
    z <- .Call("Rgraphviz_graphvizVersion")
    z
}

buildNodeList <- function(graph, nodeAttrs=list(), subGList) {
    pNodes <- list()

    nodeNames <- nodes(graph)

    if (length(nodeNames) > 0) {
        pNodes <- lapply(nodeNames, function(x) {
            new("pNode",name=x)})
        names(pNodes) <- nodeNames

        attrNames <- names(nodeAttrs)
        ## FIXME: Horribly inefficient, just trying
        ##        to get this to work for now
        for (i in 1:length(nodeNames)) {
            ## See if this node is in a subgraph
            if (length(subGList) > 0) {
                subGs <- which(unlist(lapply(subGList, function(x,y){y %in% nodes(x)},
                                             nodeNames[i])))
                if (length(subGs) == 1)
                    pNodes[[i]]@subG <- subGs ## FIXME: Need replace method
                else if (length(subGs) > 1)
                    stop("Node ", nodeNames[i], " in multiple subgraphs")
            }
            ## Get any attrs for this node
            curAttrs <- list()
            for (j in seq(along=nodeAttrs)) {
                curVal <- nodeAttrs[[j]][nodeNames[i]]
                if (!is.na(curVal))
                    curAttrs[[ attrNames[j] ]] <- as.character(curVal)
            }
            if (is.null(curAttrs$label))
                curAttrs$label <- nodeNames[i]

            ## FIXME: Need replace method
            pNodes[[i]]@attrs <- curAttrs
        }
    }

    pNodes
}


buildEdgeList <- function(graph, edgeAttrs=list(), subGList) {
    buildPEList <- function(x,y) {
        lapply(y, function(z) {new("pEdge", from=x, to=z)})
    }

    buildSubGEdgeNames <- function(subG) {
        x <- edges(subG)
        z <- names(x)
        as.vector(mapply(paste, x, z, MoreArgs=list(sep="~")))
    }

    to <- edges(graph)
    if (length(to) == 0)
        return(list())

    from <- names(to)

    pEdges <- unlist(mapply(buildPEList, from, to),
                     recursive=FALSE)

    edgemode <- edgemode(graph)

    edgeNames <- unlist(lapply(pEdges, function(x) {
        paste(from(x), to(x), sep="~")}))
    names(pEdges) <- edgeNames

    subGEdgeNames <- lapply(subGList, buildSubGEdgeNames)

    attrNames <- names(edgeAttrs)
    ## FIXME: Horribly inefficient, just trying
    ##        to get this to work for now
    for (i in 1:length(edgeNames)) {
        ## Seei f this edge is in a subgraph
        if (length(subGList) > 0) {
            subGs <- which(unlist(lapply(subGEdgeNames, function(x,y)
                                     {y %in% x}, edgeNames[i])))
            if (length(subGs) == 1)
                pEdges[[i]]@subG <- subGs ## FIXME: Need replace
                                          ## method
            else if (length(subGs) == 2)
                stop("Edge ", edgeNames[i], " is in multiple subgraphs")
        }

        ## Get any attrs for this edge
        curAttrs <- list()
        for(j in seq(along=edgeAttrs)) {
            curVal <- edgeAttrs[[j]][edgeNames[i]]
            if (!is.na(curVal))
                curAttrs[[ attrNames[j] ]] <- as.character(curVal)
        }

        ## Since we're using the arrowhead/arrowtail
        ## to specify if arrows are beign drawn or not,
        ## the default relies on the edgemode
        if (is.null(curAttrs$arrowtail))
            curAttrs$arrowtail <- "none"
        if (is.null(curAttrs$arrowhead)) {
            if (edgemode == "directed") {
                curAttrs$arrowhead <- "normal"
            } else {
                curAttrs$arrowhead <- "none"
            }
        }

        ## FIXME: Need replace method
        pEdges[[i]]@attrs <- curAttrs
    }
    pEdges
}
