\name{AgEdge-class}
\docType{class}
\alias{AgEdge-class}
\alias{AgEdge}
\alias{splines}
\alias{startArrow}
\alias{endArrow}
\alias{sp}
\alias{ep}
\alias{getSpline}
\alias{numSplines}
\title{Class "AgEdge": A class to describe an edge for a Ragraph object}
\description{  This class is used to represent edges for the
  \code{Ragraph} class.  One can retrieve various pieces of information
  as well as draw them.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AgEdge", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{splines}:}{Object of class \code{"list"} A list of
      \code{BezierCurve} objects}
    \item{\code{startArrow}:}{Object of class \code{"logical"} If there
      is an arrow at the start of the edge}
    \item{\code{endArrow}:}{Object of class \code{"logical"} If there is
    an error at the end of the edge}
    \item{\code{sp}:}{Object of class \code{"xyPoint"} The starting
      point of the edge}
    \item{\code{ep}:}{Object of class \code{"xyPoint"} The end point of
      the edge}
  }
}

\section{Methods}{
  \describe{
    \item{splines}{Returns the \code{splines} slot}
    \item{startArrow}{Returns the \code{startArrow} slot}
    \item{endArrow}{Returns the \code{endArrow} slot}
    \item{sp}{Returns the \code{sp} slot}
    \item{ep}{Returns the \code{ep} slot}
    \item{numSplines}{Returns the number of splines}
    \item{getSpline}{Convenience method to retrieve a specific spline}
    \item{show}{Displays a concise description of the object}
    \item{lines}{Draws the edge}
  }
}
\author{Jeff Gentry}
\seealso{\code{\link{Ragraph}}, \code{\link{BezierCurve}}, \code{\link{xyPoint}}}
\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
