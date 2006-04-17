\name{boundingBox-class}
\docType{class}
\alias{boundingBox-class}
\alias{boundingBox}
\alias{botLeft}
\alias{upRight}
\alias{botLeft,boundingBox-method}
\alias{upRight,boundingBox-method}
\title{Class "boundingBox": A class to describe the bounding box of a
  Ragraph}
\description{The \code{boundingBox} class is used to describe the
  dimensons of the bounding box for a laid out Ragraph}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("boundingBox", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{botLeft}:}{Object of class \code{"xyPoint"} Defines the
      bottom left point of the bounding box}
    \item{\code{upRight}:}{Object of class \code{"xyPoint"} Defines the
      upper right point of the bounding box}
  }
}

\section{Methods}{
  \describe{
    \item{botLeft}{Retrieve the botLeft slot}
    \item{upRight}{Retrieve the upRight slot}
    }
}
\author{Jeff Gentry}
\seealso{ \code{\link{Ragraph}}, \code{\link[graph]{graph-class}}}

\examples{
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, .2)
z <- agopen(g1,"foo")
x <- boundBox(z)
x

botLeft(x)
upRight(x)
}
\keyword{classes}
