\name{agopen}
\alias{agopen}
\title{A function to obtain a libgraph object}
\description{
  This function will read in a \code{graph} object and create a
  \code{Ragraph} object, returning it for use in other functions.  The
  graph represented by the \code{Ragraph} can be laidout in various formats.
}
\usage{
agopen(graph, name, kind = "AGRAPH", layout = TRUE,
       layoutType=c("dot","neato","twopi")[1], attrs=NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{graph}{An object of class \code{graphNEL}}
  \item{name}{The name of the graph}
  \item{kind}{The type of graph}
  \item{layout}{Whether to layout the graph or not}
  \item{layoutType}{Defines the layout engine.  Defaults to dot}
  \item{attrs}{Currently unused}
}
\details{
  \code{graph} is from the package \link{graph}.

  The kind parameter works as follows:
  AGRAPH - An undirected graph
  AGDIGRAPH - A directed graph
  AGRAPHSTRICT - A strict undirected graph
  AGDIGRAPHSTRICT - A strict directed graph

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

