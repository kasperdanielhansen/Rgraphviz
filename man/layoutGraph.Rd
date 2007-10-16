\name{layoutGraph}
\alias{layoutGraph}
\title{ A function to do layout graph objects }

\description{
  This is a wrapper to layout graph objects using arbitrary layout
  engines.
}

\usage{
layoutGraph(x, layoutFun = layoutGraphviz, ...)
}

\arguments{
  
  \item{x}{ A graph object }
  \item{layoutFun}{ A function that performs the graph layout and
    returns a graph object with all necessary rendering information }
  \item{\dots}{ Further arguments that are passed t \code{layoutFun} }
  
}

\details{  
  Layout of a graph and rendering are two separate processes. This
  function provides an API to use any algorithm for the layout. The only
  requirements to the \code{layoutFun} is to return a valid graph object
  with all the necessary rendering information stored in its
  \code{renderInfo} slot. This comprises

  for nodes:
  \item{nodeX, nodeY}{the locations of the nodes}
  \item{lw, rw}{the width components of the nodes, \code{lw+rw=total
      width}}
  \item{height}{the heights of the nodes}
  \item{labelX, labelY}{node label locations}
  \item{label}{node label text}
    
  for edges:
  \item{splines}{representation of the edge splines as list of
    \code{\link{BezierCurve}} objects.}
  \item{labelX, labelY}{edge label locations}
  \item{label}{edge label text}

  To indicate that this information has been added to the graph, the
  function should also set the laidout flag in the \code{graphData} slot
  to \code{TRUE} and add the bounding box information in the format of a
  two-by-two matrix as item \code{bbox} in the \code{graphData} slot.

  AT&T's \code{Graphviz} is the default layout algoritm to use when
  \code{layoutGraph} is called without a specific \code{layoutFun}
  function. See \code{\link{agopen}} for details about how to tweak
  \code{Graphviz}.
}


\value{
  An object inheriting from class \code{\link[graph]{graph}}
}

\author{ Florian Hahne, Deepayan Sarkar }


\seealso{ \code{\link[graph]{renderGraph}},
  \code{\link[graph]{graph.par}},
  \code{\link[graph]{nodeRenderInfo}},
  \code{\link[graph]{edgeRenderInfo}}
}

\examples{
library(graph)
set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, 0.8)
x <- layoutGraph(g1)
}

\keyword{ graphs }

