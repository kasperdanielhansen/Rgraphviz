\name{graphvizVersion}
\alias{graphvizVersion}
\title{ A function to determine graphviz library version}
\description{
  This function will query the graphviz libraries that the package was
  built against and report what version of Graphviz is being used.
}
\usage{
graphvizVersion()
}
\value{
  A list with three elements, two of class \code{numeric_version}. The first
  element named \code{installed_version} represents the version of
  Graphviz that is being used by the package. The second element named
  \code{build_version} represents the version of Graphviz that was used
  to build the package. A mismatch between these two versions may
  indicate problems.  The third element is a logical named
  \code{bundled_graphviz} and indicates if Rgraphviz is using the
  bundled Graphviz (default) or an external Graphviz.
}
\author{Jeff Gentry, modified by Kasper Daniel Hansen}
\examples{
graphvizVersion()
}
\keyword{graphs}

