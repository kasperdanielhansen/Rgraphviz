\name{xyPoint-class}
\docType{class}
\alias{xyPoint-class}
\alias{xyPoint}
\alias{getX}
\alias{getY}
\alias{getPoints}
\alias{getPoints,xyPoint-method}
\alias{getX,xyPoint-method}
\alias{getY,xyPoint-method}
\title{Class "xyPoint": A class to represent a X/Y coordinate.}
\description{This class is used to describe a coordinate in
  2-dimensional (X/Y) space}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("xyPoint", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"numeric"} The x coordinate}
    \item{\code{y}:}{Object of class \code{"numeric"} The y coordinate}
  }
}

\section{Methods}{
  \describe{
    \item{getX}{Returns the value stored in the \code{x} slot}
    \item{getY}{Returns the value stored in the \code{y} slot}
    \item{getPoints}{Returns a vector of two numerical values
      representing the \code{x} and \code{y} positions}
    \item{show}{Display information about the object in a concise fashion}
  }
}
\author{Jeff Gentry}
\examples{
   z <- new("xyPoint", x=150, y=30)
   z
   getPoints(z)
}
\keyword{classes}
