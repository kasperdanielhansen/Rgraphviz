\name{agopenSimple}
\alias{agopenSimple}
\title{A function to obtain a Ragraph object}
\description{
  This function will read in a \code{graphNEL} object and create a
  \code{Ragraph} object, returning it for use in other functions.  The
  graph represented by the \code{Ragraph} can be laidout in various formats.
}
\usage{
agopenSimple(graph, name, kind = NULL, edgeMode=edgemode(graph),
       subGList=list(), recipEdges=c("combined", "distinct"))
}
\arguments{
  \item{graph}{An object of class \code{graphNEL}}
  \item{name}{The name of the \code{Ragraph}}
  \item{kind}{The type of graph}
  \item{subGList}{A list describing subgraphs for the graph parameter}
  \item{edgeMode}{Whether the graph is directed or undirected}
  \item{recipEdges}{How to handle reciprocated edges, defaults to
    \code{combined}, TODO: use this }
}
\details{
  \code{graph} is from the package \code{\link[graph]{graph-class}}.

  The \code{edgeMode} argument specifies whether the graph is to be laid
  out with directed or undirected edges.  This parameter defaults to the
  edgemode of the \code{graph} argument. 

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

  The \code{subGList} parameter is a list describing any subgraphs,
  where each element represents a subgraph and is itself a list with up
  to three elements.  The first element, \code{graph} is required and
  contains the actual \code{graph} object for the subgraph.  The second
  element, \code{cluster} is a logical value indicating if this is a
  \code{cluster} or a \code{subgraph} (a value of \code{TRUE} indicates
  a cluster, which is also the default value if this element is not
  specified).  In Graphviz, subgraphs are more of an organizational
  mechanism, whereas clusters are laid out separately from the main
  graph and then later inserted.  The last element of the list,
  \code{attrs} is used if there are any attributes for this subgraph.
  This is a named vector where the names are the attributes and the
  elements are the values for those attributes.

  The \code{recipEdges} argument can be used to specify how to handle
  reciprocal edges.  The default value, \code{combined} will combine any
  reciprocated edges into a single edge (and if the graph is directed,
  will by default place an arrowhead on both ends of the edge), while
  the other option is \code{distinct} which will draw to separate
  edges.  Note that in the case of an undirected graph, every edge of a
  \code{graphNEL} is going to be reciprocal due to implementation issues.
}
\value{
  An object of class \code{Ragraph}
}
\references{http://www.research.att.com/sw/tools/graphviz/}
\author{Li Long <li.long@isb-sib.ch>}
\seealso{\code{\link{graphLayout}},
  \code{\link{Ragraph}}, \code{\link{plot}}}
\examples{
set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, .2)
z <- agopenSimple(g1,name="foo")
plot(z, "twopi")
}
\keyword{graphs}

