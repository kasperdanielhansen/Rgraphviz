  agopen <- function(graph, name, nodeLabels=nodes(graph),
                     kind=NULL, layout=TRUE,
                     layoutType=c("dot","neato","twopi")[1],
                     attrs=getDefaultAttrs(layoutType,
                     edgemode(graph)), subGList,
                     edgeLabels=list()) {

      checkAttrs(attrs)

      if (is.null(kind)) {
          ## Determine kind from the graph object
          outK <- switch(edgemode(graph),
                         "undirected"=0,
                         "directed"=1,
                         0)
      }
      else {
          ## Use the specified 'kind' parameter.
          outK <- switch(kind,
                         "AGRAPH"=0,
                         "AGDIGRAPH"=1,
                         "AGRAPHSTRICT"=2,
                         "AGDIGRAPHSTRICT"=3,
                         stop(paste("Incorrect kind parameter:",kind)))
      }

      edgeMtrx <- graph2graphviz(graph)

      nodes <- nodes(graph)

      ## We allow for users to pass in either 1
      ## label which is used for all nodes, or a vector
      ## with length equal to the length of nodes,
      ## specifying the labels in order.
      nNL <- length(nodeLabels)

      if (nNL == 1)
          nodeLabels <- rep(nodeLabels, length(nodes))
      else
          if (length(nodeLabels) != length(nodes))
              stop(paste("nodeLabels must be the same",
                         "length as the number of nodes"))

      nL <- nodeLabels
      if (attrs$node$fixedsize)
          nL <- rep(nL[match(max(nchar(nL)), nchar(nL))],
                    length(nodes))

      if ((is.list(edgeLabels))&&(length(edgeLabels) == length(nodes))) {
          if (!all(unlist(lapply(edgeLabels,is.character))))
              stop("edgeLabel list can only contain character vectors")
      }
      else if (length(edgeLabels) > 1)
          stop("edgeLabels must be either ",
               "empty, length one, or a list with the length of nodes")
      else if (length(edgeLabels) == 1)
          edgeLabels <- repEdgeLabels(edgeLabels, graph)

      subGs <- vector(mode="character")

      ## A vector to map nodes to subgraphs
      nodeSubs <- vector(length=length(nodes), mode="numeric")
      names(nodeSubs) <- nodes

      ## A vector to map edges to subgraphs
      edgeSubs <- rep(0,length(unlist(edges(graph))))

      ## Store these in an environment to facilitate lapply() and
      ## friends in the subgraph calculations
      arrEnv <- new.env()
      assign("nodeSubs",nodeSubs,env=arrEnv)
      assign("edgeSubs",edgeSubs,env=arrEnv)

      if ((!missing(subGList))&&(length(subGList) > 0)) {
          ## graphviz expects the names of clusters to start with
          ## 'cluster', so create proper names for the subgrpahs
          subGs <- paste("cluster_",1:length(subGList),sep="")

          edgeFromTo <- edgeMtrx[1:2,]
          for (i in 1:length(subGList)) {
              subNodes <- nodes(subGList[[i]])
              ## Calculate which nodes belong to which subgraph
              ## and track that information in the nodeSubs vector
              lapply(subNodes,function(x,arrEnv,i) {
                  nodeSubs <- get("nodeSubs",arrEnv)
                  if (nodeSubs[x] != 0)
                      stop("Duplicated nodes in subgraphs")
                  else {
                      nodeSubs[x] <- i
                      assign("nodeSubs",nodeSubs,env=arrEnv)
                  }
                  return(NULL)
              },arrEnv,i)

              ## Now do the edges
              subEdgeL <- edgeL(subGList[[i]])
              fromEdgeNames <- names(subEdgeL)
              for (j in 1:length(subEdgeL)) {
                  toEdges <- subEdgeL[[j]]$edges
                  curEdgeNum <- as.integer(match(fromEdgeNames[j],
                                           nodes))
                  lapply(toEdges,
                         function(x, arrEnv, toEdges,
                                  curEdgeNum, edgeFromTo, i) {
                             tmp <- matrix(c(curEdgeNum,x),nrow=2,ncol=1)
                             tmpMatches <- apply(edgeFromTo,2,"==",tmp)
                             tmpMatches <- apply(tmpMatches,2,all)
                             if (any(tmpMatches)) {
                                 whichEdge <- which(tmpMatches)[1]
                                 edgeSubs <- get("edgeSubs",arrEnv)
                                 if (edgeSubs[whichEdge] != 0)
                                     stop("Duplicated edges in subgraphs")
                                 else {
                                     edgeSubs[whichEdge] <- i
                                     assign("edgeSubs",edgeSubs,env=arrEnv)
                                 }
                             }
                             return(NULL)
                         }, arrEnv, toEdges, curEdgeNum, edgeFromTo, i)
              }
          }
      }

      ## all attrs must be character strings going into C,
      ## graphviz wants all attrs to be char*
      attrs <- lapply(attrs, function(x){lapply(x,as.character)})

      g <- .Call("Rgraphviz_agopen", as.character(name),
                 as.integer(outK), as.vector(nodes),
                 as.character(nodeLabels), as.list(edgeLabels),
                 as.integer(edgeMtrx[1,]), as.integer(edgeMtrx[2,]),
                 as.double(edgeMtrx[3,]),
                 as.integer(get("edgeSubs",env=arrEnv)),
                 as.integer(get("nodeSubs",env=arrEnv)),
                 as.character(subGs), as.list(attrs))
      g@layoutType <- layoutType
      g@edgemode <- edgemode(graph)
      g@nodeLabels <- nodeLabels
      g@edgeLabels <- edgeLabels
      g@nodeNames <- nodes

      if (layout)
          return(layoutGraph(g,layoutType))
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

layoutGraph <- function(graph, layoutType=c("dot","neato","twopi")[1]) {
    if (inherits(graph,"graphNEL"))
        stop("Please use function agopen() for graphNEL objects")
    if (!inherits(graph,"Ragraph"))
        stop("Object is not of class Ragraph")

    type <- switch(layoutType,
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
