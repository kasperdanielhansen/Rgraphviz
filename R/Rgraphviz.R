  agopen <- function(graph, name, nodeLabels=nodes(graph),
                     kind="AGRAPH", layout=TRUE,
                     layoutType=c("dot","neato","twopi")[1],
                     attrs=NULL, subGList, edgeLabels) {

      outK <- switch(kind,
                     "AGRAPH"=0,
                     "AGDIGRAPH"=1,
                     "AGRAPHSTRICT"=2,
                     "AGDIGRAPHSTRICT"=3,
                     stop(paste("Incorrect kind parameter:",kind)))

      edgeMtrx <- graph2graphviz(graph)

      nodes <- nodes(graph)

      if (missing(edgeLabels))
          edgeLabels <- list()
      else {
          if (!is.list(edgeLabels))
              stop("edgeLabels must be a list")

          if (!all(unlist(lapply(edgeLabels,is.character))))
              stop("edgeLabel list can only contain character vectors")
      }

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
                             tmp <- matrix(c(curEdgeNum,x),nrow=1,ncol=2)
                             tmpMatches <- apply(edgeFromTo,1,"==",tmp)
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

      g <- .Call("Rgraphviz_agopen", as.character(name),
                 as.integer(outK), as.vector(nodes),
                 as.character(nodeLabels), as.list(edgeLabels),
                 as.integer(edgeMtrx[1,]), as.integer(edgeMtrx[2,]),
                 as.double(edgeMtrx[3,]), as.integer(get("edgeSubs",env=arrEnv)),
                 as.integer(get("nodeSubs",env=arrEnv)),
                 as.character(subGs), as.list(attrs))

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
