
buildTestNodes <- function() {
       nodes <- list()
      nodes[[1]] <- new("pNode",name="foo")
      nodes[[2]] <- new("pNode", name="blah", label="blah2")
      nodes[[3]] <- new("pNode", name="bar")
      nodes[[4]] <- new("pNode", name="blat")
      nodes[[5]] <- new("pNode", name="5")
      nodes[[6]] <- new("pNode", name="6")
      nodes[[7]] <- new("pNode", name="7")
       nodes[[8]] <- new("pNode", name="8", label="test")
       nodes[[9]] <- new("pNode", name="9", label="chirac")
       nodes[[10]] <- new("pNode", name="10", label="jacques")
nodes
   }

buildTestEdges <- function() {
      edges <- list()
      edges[[1]] <- new("pEdge", from="foo", to="blah", label="test")
      edges[[2]] <- new("pEdge", from="foo", to="bar")
      edges[[3]] <- new("pEdge", from="bar", to="blat")
      edges[[4]] <- new("pEdge", from="5", to="6")
      edges[[5]] <- new("pEdge", from="7", to="bar")
      edges[[6]] <- new("pEdge", from="bar", to="7")
      edges[[7]] <- new("pEdge", from="blat", to="8", label="new edge")
      edges[[8]] <- new("pEdge", from="blah", to="8", label="another")
      edges[[9]] <- new("pEdge", from="9", to="10")
      edges[[10]] <- new("pEdge", from="10", to="9")
      edges[[11]] <- new("pEdge", from="9", to="5")
      edges[[12]] <- new("pEdge", from="10", to="foo")
      edges[[13]] <- new("pEdge", from="9", to="7")
      edges[[14]] <- new("pEdge", from="7", to="blah")
      edges
  }

agopen <- function(graph, name, kind=NULL, layout=TRUE,
                   layoutType=c("dot","neato","twopi")[1],
                   attrs=getDefaultAttrs(layoutType), subGList) {

      checkAttrs(attrs)

      ## !!!!!!
      nodes <- buildTestNodes()
      edges <- buildTestEdges()

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

      ### FIXME: Subgraph stuff needs to go here.

      ## all attrs must be character strings going into C,
      ## graphviz wants all attrs to be char*
      ## FIXME: so shouldn't we do that in C?
      attrs <- lapply(attrs, function(x){lapply(x,as.character)})

      g <- .Call("Rgraphviz_agopen", as.character(name),
                 as.integer(outK), as.list(nodes),
                 as.list(edges), as.list(attrs))
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
