\name{agopen}
\alias{agopen}
\title{A function to obtain a libgraph object}
\description{
  This function will read in a \code{graph} object and create a
  \code{libgraph} external reference in C.  This reference can then be
  used for other functions.
}
\usage{
agopen(graph, name, kind = 0, layout = TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{graph}{An object of class \code{graphNEL}}
  \item{name}{The name of the graph}
  \item{kind}{The type of graph}
  \item{layout}{Whether to layout the graph or not}
}
\details{
  \code{graph} is from the package \link{graph}.

  The kind parameter works as follows:
  0 - An undirected graph
  1 - A directed graph
  2 - A strict undirected graph
  3 - A strict directed graph

  Strict graphs do not allow self arcs or multi-edges.

  If \code{layout} is set to \code{TRUE}, then the \code{libgraph}
  routines are used to compute the layout locations for the graph.
  Otherwise the graph is returned without layout information.
}
\value{
  An object of class \code{Ragraph}
}
\references{http://www.research.att.com/sw/tools/graphviz/}
\author{Jeff Gentry}
\seealso{\code{\link{layoutGraph}},
  \code{\link{Ragraph}}, \code{\link{plotGraph}}}
\examples{
set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, .2)
z <- agopen(g1,"foo")
z
}
\keyword{utilities}

