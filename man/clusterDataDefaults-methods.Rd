\name{clusterDataDefaults-methods}
\docType{methods}
\alias{clusterDataDefaults-methods}
\alias{clusterDataDefaults<--methods}
\alias{clusterDataDefaults}
\alias{clusterDataDefaults<-}

\title{Get and set default attributes for a cluster of an Ragraph}

\usage{
clusterDataDefaults(self, cluster)
clusterDataDefaults(self, cluster, attr) <- value
}
\arguments{
  \item{self}{A \code{\link{Ragraph-class}} instance}
  \item{cluster}{cluster number }
  \item{attr}{A \code{character} vector of length one giving the name of
    the attribute}
  \item{value}{An R class to use as the default value for the specified
    attribute}
}
\description{
  Get/Set default values for attributes associated with a cluster of a graph.
}

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}


\keyword{methods}

