\name{AgTextLabel-class}
\docType{class}
\alias{AgTextLabel-class}
\alias{labelText}
\alias{labelLoc}
\alias{labelJust}
\alias{labelWidth}
\alias{labelColor}
\alias{labelFontsize}
\alias{labelFontsize,AgTextLabel-method}
\alias{labelColor,AgTextLabel-method}
\alias{labelText,AgTextLabel-method}
\alias{labelLoc,AgTextLabel-method}
\alias{labelJust,AgTextLabel-method}
\alias{labelWidth,AgTextLabel-method}
\alias{drawTxtLabel}
\title{Class "AgTextLabel": Represents a graphviz text label}
\description{ This class is used to represent the textlabel object in
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
    \item{\code{labelColor}:}{Object of class \code{"character"} The
      color to print the label} 
    \item{\code{labelFontsize}:}{Object of class \code{"numeric"} The
      font size for the label}
  }
}
\section{Methods}{
  \describe{
    \item{labelText}{Retrieves the labelText slot}
    \item{labelLoc}{Retrieves the labelLoc slot}
    \item{labelJust}{Retrieves the labelJust slot}
    \item{labelWidth}{Retrieves the labelWidth slot}
    \item{labelColor}{Retrieves the labelColor slot}
    \item{labelFontsize}{Retrieves the labelFontsize slot}
  }
}
\author{Jeff Gentry}
\seealso{\code{\link{AgEdge-class}}, \code{\link{AgNode-class}}}
\examples{
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, .2)
z <- agopen(g1,"foo")
x <- AgEdge(z)  ## list of AgEdge objects
x[[1]]  ## AgEdge
a <- txtLabel(x[[1]])
a ## txtLabel object
}
\keyword{classes}
