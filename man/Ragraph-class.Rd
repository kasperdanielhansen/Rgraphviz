\name{Ragraph-class}
\docType{class}
\alias{Ragraph-class}
\alias{Ragraph}
\alias{laidout}
\alias{agraph}
\alias{nodeLocs}
\title{Class "Ragraph": A class to handle libgraph representations}
\description{Class \code{Ragraph} is used to handle libgraph
  representations of R graph objects.}
\section{Objects from the Class}{
Objects can be created by calls to the function \code{agopen}.
}
\section{Slots}{
  \describe{
    \item{\code{agraph}:}{Object of class \code{"externalptr"}: A C
      based structure containing the libgraph information}
    \item{\code{laidout}:}{Object of class \code{"logical"}: Whether or
      not this graph has been laid out or not.}
    \item{\code{nodes}:}{Object of class \code{"list"}: A list of
      \code{NodePosition} objects.}
    \item{\code{AgEdge}:}{Object of class \code{"list"}: A list of
      \code{AgEdge} objects.}
    \item{\code{boundBox}:}{Object of class \code{"boundBox"}: Defines
      the bounding box of the plot.}
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Ragraph")}: A brief summary of
      the contents}
    \item{agraph}{\code{signature(object = "Ragraph")}: Returns the
      external libgraph pointer}
    \item{laidout}{\code{signature(object = "Ragraph")}: Returns the
      \code{laidout} slot}
    \item{nodes}{\code{signature(object = "Ragraph")}: Returns the node
      list.}
    \item{boundBox}{\code{signature(object = "Ragraph")}: Returns the
      bounding box.}
    \item{AgEdge}{\code{signature(object = "Ragraph")}: Returns the edge
      list.}
    \item{getNodeLocs}{\code{signature(object = "Ragraph")}: Returns a
      list with X and Y positions of all nodes, used as a convenience}
  }
}
\author{Jeff Gentry}

\seealso{\code{\link{agopen}}}

\examples{
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, .2)
z <- agopen(g1,"foo")
z
}
\keyword{classes}
