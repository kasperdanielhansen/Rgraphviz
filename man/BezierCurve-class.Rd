\name{BezierCurve-class}
\docType{class}
\alias{BezierCurve-class}
\alias{BezierCurve}
\alias{cPoints}
\alias{pointList}
\alias{bezierPoints}
\alias{bezier}
\title{Class "BezierCurve": A class to describe a Bezier curve}
\description{  This class is used to represent a Bezier curve in R,
  which can then be used for other applications, plotted, etc}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BezierCurve", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{cPoints}:}{Object of class \code{"list"}: A list of
      \code{xyPoint} objects, representing control points for the curve}
  }
}

\section{Methods}{
  \describe{
    \item{\code{cPoints}:}{Returns the \code{cPoints} slot}
    \item{\code{pointList}:}{Returns a list of all points having been
      processed with teh \code{getPoints} method of \code{xyPoint}}
    \item{\code{bezierPoints}:}{Returns a matrix giving x & y points (by
      column) for the complete Bezier curve}
    \item{\code{lines}:}{Draws the Bezier curve}
    \item{\code{show}:}{Provides a concise display of information}
  }
}
\author{Jeff Gentry}
\seealso{\code{\link{xyPoint}}}
\examples{
}
\keyword{classes}
