\name{buildNodeList}
\alias{buildNodeList}
\alias{buildEdgeList}
\title{A function to build lists of node and edge objects}
\description{
  These functions can be used to generate lists of \code{pNode} and
  \code{pEdge} objects from an object of class \code{graph}.  These
  lists can then be sent to Graphviz to initialize and layout the graph
  for plotting.
}
\usage{
buildNodeList(graph, nodeAttrs = list(), subGList=list())
buildEdgeList(graph, edgeAttrs = list(), subGList=list())
}
\arguments{
  \item{graph}{An object of class \code{graph}}
  \item{nodeAttrs}{A list of attributes for specific nodes}
  \item{edgeAttrs}{A list of attributes for specific edges}
  \item{subGList}{A list of any subgraphs to be used in Graphviz}
}
\details{
  These functions will take either the nodes or the edges of the
  specified graph and generate a list of either \code{pNode} or
  \code{pEdge} objects.

  The \code{nodeAttrs} and \code{edgeAttrs} attribute lists are to be
  used for cases where one wants to set an attribute on a node or an
  edge that is not the default.  In both cases, these are lists with the
  names of the elements corresponding to a particular attribute and the
  elements containing a named vector - the names of the vector are names
  of either node or edge objects and the values in the vector are the
  values for this attribute.

  Note that with the \code{edgeAttrs} list, the name of the edges are in
  a particular format where an edge between x and y is named \code{x~y}.
  Note that even in an undirected graph that \code{x~y} is not the same
  as \code{y~x} - the name must be in the same order that the edge was
  defined as having. 

  The \code{subGraph} argument can be used to specify a list of
  subgraphs that one wants to use for this plot.  The \code{buildXXXList}
  functions will determine if a particular node or edge is in one of the
  subgraphs and note that in the object.
}
\value{
  A list of class \code{pNode} or \code{pEdge} objects.
}
\author{Jeff Gentry}
\seealso{\code{\link{agopen}}, \code{link{plot.graph}},
  \code{\link{pNode}},\code{\link{pEdge}}}
\examples{
   set.seed(123)
   V <- letters[1:10]
   M <- 1:4
   g1 <- randomGraph(V, M, .2)

   z <- buildEdgeList(g1)
   x <- buildNodeList(g1)
}
\keyword{graphs}
