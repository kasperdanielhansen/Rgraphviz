\name{getDefaultAttrs}
\alias{checkAttrs}
\alias{getDefaultAttrs}
\title{Functions to generate and check global attribute lists}
\description{
  The \code{getDefaultAttrs} function can be used to generate a default
  global attribute list for Graphviz.  The \code{checkAttrs} function
  can be used to verify that such a list is valid for use.
}
\usage{
getDefaultAttrs(layoutType=c("dot", "neato", "twopi")[1])
checkAttrs(attrs)
}
\arguments{
  \item{layoutType}{The layout method being used}
  \item{attrs}{ An attribute list of graphviz attributes}
}
\details{
  The \code{getDefaultAttrs} function generates a four element list
  (elements being \dQuote{graph}, \dQuote{cluster}, \dQuote{node} and
  \dQuote{edge}).  Contained in each is another list where the element
  names correspond to attributes and the value is the value for that
  attribute.  This list can be used to set global attributes in
  Graphviz, and the exact list returned by \code{getDefaultAttrs}
  represents the values that are used as basic defaults.

  The \code{checkAttrs} function can be used to verify that a global
  attribute list is properly formed.
}
\author{Jeff Gentry}
\seealso{\code{\link{agopen}}, \code{\link{plot.graph}}}
\examples{
  z <- getDefaultAttrs()
  checkAttrs(z)
}
\keyword{graphs}
