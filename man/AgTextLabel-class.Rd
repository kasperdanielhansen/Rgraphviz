\name{AgTextLabel-class}
\docType{class}
\alias{AgTextLabel-class}
\alias{labelText}
\alias{labelLoc}
\alias{labelJust}
\alias{labelWidth}
\alias{labelText,AgTextLabel-method}
\alias{labelLoc,AgTextLabel-method}
\alias{labelJust,AgTextLabel-method}
\alias{labelWidth,AgTextLabel-method}
\title{Class "AgTextLabel": Represents a textlabel_t}
\description{ This class is used to represent a 'textlabel_t' object in
  C from the Graphviz library}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AgTextLabel", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{labelText}:}{Object of class \code{"character"} The
      actual label text}
    \item{\code{labelLoc}:}{Object of class \code{"xyPoint"} The
      location of the label }
    \item{\code{labelJust}:}{Object of class \code{"character"} The
      justification of the label }
    \item{\code{labelWidth}:}{Object of class \code{"integer"} The width
    of the label}
  }
}
\section{Methods}{
  \describe{
    \item{labelText}{Retrieves the labelText slot}
    \item{labelLoc}{Retrieves the labelLoc slot}
    \item{labelJust}{Retrieves the labelJust slot}
    \item{labelWidth}{Retrieves the labelWidth slot}
    }
}
\author{Jeff Gentry}
\seealso{\code{\link{AgEdge-class}}}}
\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
