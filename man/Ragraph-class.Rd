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
    \item{\code{nodeLocs}:}{Object of class \code{"matrix"}: X/Y
      coordinates for the nodes (if the graph has been laid out)}
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
    \item{nodeLocs}{\code{signature(object = "Ragraph")}: Returns the
      node location matrix}
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
