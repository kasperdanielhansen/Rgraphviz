.initGraphMethods <- function(where) {
    setMethod("plot", c("graphNEL", "missing"),
              function(x,y,...){
                  require(Rgraphviz)

                  ## Get kind from graph in future
                  g = agopen(x, name, layout=TRUE)

                  edges <- edges(x)
                  names <- names(edges)

                  if (length(names) > 0) {
                      nodeLocs <- getNodeLocs(g)

                      nodeX <- nodeLocs[["x"]]
                      nodeY <- nodeLocs[["y"]]

                      ## Get the radii of the nodes.  FOr now we're just
                      ## implementing circles
                      rad <- unlist(lapply(nodes(g), getNodeRW))

                      ## Get the upper right X,Y point of the bounding
                      ## box for the graph
                      ur <- upRight(boundBox(g))

                      ## Set up the plot region, plot the edges, then the nodes,
                      ## and finally the node labels
                      par(pty="s")
                      outLim <- max(getY(ur), getX(ur))
                      plot(NA,NA,xlim=c(0,outLim), ylim=c(0,outLim),
                           type="n",main=NULL,xlab="",ylab="",xaxt="n",
                           yaxt="n")
                      q <- lapply(edgePoints(g), plot)
                      symbols(nodeX, nodeY, circles=rad, inches=FALSE,
                              bg="white",add=TRUE)

                      text(nodeX,nodeY, names, cex=2)
                  }
                  else {
                      stop("No nodes in graph")
                  }
              }, where=where)
}
