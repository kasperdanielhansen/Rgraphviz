\name{agopen}
\alias{agopen}
\title{A function to obtain a libgraph object}
\description{
  This function will read in a \code{graph} object and create a
  \code{Ragraph} object, returning it for use in other functions.  The
  graph represented by the \code{Ragraph} can be laidout in various formats.
}
\usage{
agopen(graph, name, nodeLabels=nodes(graph), kind = NULL, layout = TRUE,
       layoutType=c("dot","neato","twopi")[1], attrs=NULL,
       subGList, edgeLabels)
}
\arguments{
  \item{graph}{An object of class \code{graphNEL}}
  \item{name}{The name of the graph}
  \item{nodeLabels}{Labels for the nodes}
  \item{kind}{The type of graph}
  \item{layout}{Whether to layout the graph or not}
  \item{layoutType}{Defines the layout engine.  Defaults to dot}
  \item{attrs}{A list of graphviz attributes}
  \item{subGList}{A list describing subgraphs for the graph parameter}
  \item{edgeLabels}{A list describing labels for the edges}
}
\details{
  \code{graph} is from the package \code{\link{graph-class}}.

  The kind parameter works as follows:
  \describe{
    \item{NULL:}{Determine the direction of the graph from the \code{graph}
      object.  This is the default and the recommended method.} 
    \item{AGRAPH:}{An undirected graph}
    \item{AGDIGRAPH:}{A directed graph}
    \item{AGRAPHSTRICT:}{A strict undirected graph}
    \item{AGDIGRAPHSTRICT:}{A strict directed graph}
  }
  Strict graphs do not allow self arcs or multi-edges.

  If \code{layout} is set to \code{TRUE}, then the \code{libgraph}
  routines are used to compute the layout locations for the graph.
  Otherwise the graph is returned without layout information.

  The \code{attrs} argument takes a list, with three sublists -
  \code{graph}, \code{node}, and \code{edge}.  Each of these sublists
  will correspond to graph-wide, node-wide or edge-wide attributes
  to be set, with the name of the element corresponding to the attribute
  name and the value of the element being the value.

  If the \code{subGList} parameter is used, every element in that list
  must be a \code{graph} that is a proper subset of the actual
  \code{graph} parameter.  These subgraphs are then noted to the
  underlying Graphviz software to be plotted as a cluster.

  The \code{edgeLabels} parameter, if supplied, specifies custom labels
  for edges.  The format of the list is that each element specifies a
  \code{from} node, and then within those elements will be named
  character vectors, where the names of the vector elements are the
  \code{to} nodes.  The values of the vector represent the edge labels.
}
\value{
  An object of class \code{Ragraph}
}
\references{http://www.research.att.com/sw/tools/graphviz/}
\author{Jeff Gentry}
\seealso{\code{\link{layoutGraph}},
  \code{\link{Ragraph}}, \code{\link{plot}}}
\examples{
set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, .2)
z <- agopen(g1,"foo")
z
z <- agopen(g1,"foo",layoutType="neato")
}
\keyword{utilities}

