\name{agopen}
\alias{agopen}
\title{A function to obtain a libgraph object}
\description{
  This function will read in a \code{graph} object and create a
  \code{Ragraph} object, returning it for use in other functions.  The
  graph represented by the \code{Ragraph} can be laidout in various formats.
}
\usage{
agopen(graph, name, kind = NULL, layout = TRUE,
       layoutType=c("dot","neato","twopi")[1],
       attrs=getDefaultAttrs(layoutType),
       nodeAttrs=list(), edgeAttrs=list(),
       subGList)
}
\arguments{
  \item{graph}{An object of class \code{graphNEL}}
  \item{name}{The name of the graph}
  \item{kind}{The type of graph}
  \item{layout}{Whether to layout the graph or not}
  \item{layoutType}{Defines the layout engine.  Defaults to dot}
  \item{attrs}{A list of graphviz attributes}
  \item{nodeAttrs}{A list of specific node attributes}
  \item{edgeAttrs}{A list of specific edge attributes}
  \item{subGList}{A list describing subgraphs for the graph parameter}
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

  If the \code{subGList} parameter is used, every element in that list
  must be a \code{graph} that is a proper subset of the actual
  \code{graph} parameter.  These subgraphs are then noted to the
  underlying Graphviz software to be plotted as a cluster.

  For a description of \code{attrs}, \code{nodeAttrs} and
  \code{edgeAttrs}, see the \code{\link{Ragraph}} man page.
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
\keyword{graphs}

