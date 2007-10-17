\name{renderGraph}
\docType{methods}
\alias{renderGraph}
\alias{renderGraph,graph-method}
\title{ Render a laid out graph object}
\description{
  This method uses the \code{renderInfo} slot of a graph object to
  render it on a plotting device.
}
\usage{
renderGraph(x, ...)
}

\arguments{
  \item{x}{ An object derived from class \code{graph}}
  \item{\dots}{further arguments}
}
\details{
  This method can render graph objects that have previously been laid
  out using the function \code{\link{layoutGraph}}. The details for user
  defined node drawing remain to be decided.
  The graph method has the following additional arguments:
  \item{drawNodes}{ A function that is used for the node rendering. The
    details of its API are still undecided. Defaults to
    \code{renderNodes}}
  \item{drawEdges}{ A function that is used for the edge
    rendering. Defaults to \code{renderNodes}}
  \item{graph.par}{A list of rendering paramters to use as default}
}
\value{
  This method is called for its side effect.
}

\author{ Florian Hahne}

\seealso{ \code{\link{layoutGraph}} }
\examples{
library(graph)
set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, 0.8)
x <- layoutGraph(g1)
renderGraph(x)
}

\keyword{ methods}
