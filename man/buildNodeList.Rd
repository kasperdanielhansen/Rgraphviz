\name{buildNodeList}
\alias{buildNodeList}
\alias{buildEdgeList}
\alias{edgeL,clusterGraph-method}
\alias{edgeL,distGraph-method}
\title{A function to build lists of node and edge objects}
\description{
  These functions can be used to generate lists of \code{pNode} and
  \code{pEdge} objects from an object of class \code{graph}.  These
  lists can then be sent to Graphviz to initialize and layout the graph
  for plotting.
}
\usage{
buildNodeList(graph, nodeAttrs = list(), subGList=list(), defAttrs=list())
buildEdgeList(graph, recipEdges=c("combined", "distinct"),
              edgeAttrs = list(), subGList=list(), defAttrs=list())
}
\arguments{
  \item{graph}{An object of class \code{graph}}
  \item{nodeAttrs}{A list of attributes for specific nodes}
  \item{edgeAttrs}{A list of attributes for specific edges}
  \item{subGList}{A list of any subgraphs to be used in Graphviz}
  \item{recipEdges}{How to deal with reciprocated edges}
  \item{defAttrs}{A list of attributes used to specify defaults.}
}
\details{
  These functions will take either the nodes or the edges of the
  specified graph and generate a list of either \code{pNode} or
  \code{pEdge} objects.

  The \code{recipEdges} argument can be used to specify how to handle
  reciprocal edges.  The default value, \code{combined} will combine any
  reciprocated edges into a single edge (and if the graph is directed,
  will by default place an arrowhead on both ends of the edge), while
  the other option is \code{distinct} which will draw to separate
  edges.  Note that in the case of an undirected graph, every edge of a
  \code{graphNEL} is going to be reciprocal due to implementation issues.

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

  The \code{defAttrs} list is a list used to specify any default values
  that one wishes to use.  The element names corresponde to the
  attribute and the value is the default for that particular attribute.

  If there is no default specified in \code{defAttrs} for an attribute
  declared in \code{nodeAttrs} or \code{edgeAttrs}, then the latter must
  have a value for every node or edge in the graph.  Otherwise, if a
  default is supplied, that value is used for any node or edge not
  explicitly defined for a particular attribute.
}
\value{
  A list of class \code{pNode} or \code{pEdge} objects.
}
\author{Jeff Gentry}
\seealso{\code{\link{agopen}}, \code{\link{plot.graph}},
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
