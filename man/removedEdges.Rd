\name{removedEdges}
\alias{removedEdges}
\title{A Function To List Removed Edges}
\description{
  This function can be used to retrieve a numerical vector which will
  describe which edges in a graph would be removed if \code{recipEdges}
  is set to \code{combined} during plotting.
}
\usage{
removedEdges(graph)
}
\arguments{
  \item{graph}{An object of class \code{graph} or \code{Ragraph}, the
    graph to perform this operation on}
}
\details{
  This function will simply detect which (if any) edges in a graph would
  be removed during combination of reciprocated edges.
}
\value{
  A numerical vector, where the values correspond to removed edges.
}
\author{Jeff Gentry}
\seealso{\code{\link{edgeNames}}, \code{\link{agopen}}, \code{\link{buildEdgeList}}}
\examples{
set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, .2)
removedEdges(g1)
}
\keyword{graphs}
