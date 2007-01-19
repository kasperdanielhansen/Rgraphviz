\name{graphDataDefaults-methods}
\docType{methods}
\alias{graphDataDefaults-methods}
\alias{graphDataDefaults<--methods}
\alias{graphDataDefaults}
\alias{graphDataDefaults<-}

\title{Get and set default attributes for an Ragraph}

\usage{
graphDataDefaults(self)
graphDataDefaults(self, attr) <- value
}
\arguments{
  \item{self}{A \code{\link{Ragraph-class}} instance}
  \item{attr}{A \code{character} vector of length one giving the name of
    the attribute}
  \item{value}{An R class to use as the default value for the specified
    attribute}
}
\description{
  Set default values for attributes associated with a graph.
}
\keyword{methods}

