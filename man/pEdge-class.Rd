\name{pEdge-class}
\docType{class}
\alias{pEdge-class}
\alias{from}
\alias{to}
\alias{from,pEdge-method}
\alias{to,pEdge-method}

\title{Class "pEdge": A class to represent an edge}
\description{This class is used to represent all necessary information
  to plot an edge in Graphviz}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pEdge", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{from}:}{Object of class \code{"character"}: The name of
      the node on the tail of this edge.}
    \item{\code{to}:}{Object of class \code{"character"}: The name of
      the node on the head of this edge.}
    \item{\code{attrs}:}{Object of class \code{"list"}: A list of
      attributes specific to this edge.}
    \item{\code{subG}:}{Object of class \code{"integer"}: Which subgraph
      this edge is a part of.}
  }
}
\section{Methods}{
  \describe{
    \item{from}{\code{signature(object = "pEdge")}: Retrieves the
      \code{from} slot of this edge}
    \item{to}{\code{signature(object = "pEdge")}: Retrieves the
      \code{to} slot of this edge}
  }
}
\details{
  The \code{attrs} slot is a named list, where the names correspond to
  attributes and the values in the list correspond to the value for that
  element's attribute.

  The \code{subG} slot describes which subgraph this edge is a part of.
  A value of \code{0} implies that the edge is not a member of any subgraph.
}
\author{R. Gentleman and Jeff Gentry}
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{pNode}}, \code{\link{agopen}}}
\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
