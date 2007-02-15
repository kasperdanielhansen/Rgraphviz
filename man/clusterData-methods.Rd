\name{clusterData-methods}
\docType{methods}
\alias{clusterData-methods}
\alias{clusterData<--methods}
\alias{clusterData}
\alias{clusterData<-}

\title{Get and set attributes for a cluster of an Ragraph object}

\usage{
clusterData(self, cluster, attr)
clusterData(self, cluster, attr, defval) <- value
}
\arguments{
  \item{self}{A \code{Ragraph-class} instance}
  \item{cluster}{cluster number}
  \item{attr}{A \code{character} vector of length one specifying the
  name of a cluster attribute}
  \item{defval}{A \code{character} vector to store as the default attribute value}
  \item{value}{A \code{character} vector to store as the attribute value}
}

\description{
  Attributes of a graph can be accessed using \code{clusterData}.  
  The attributes must be defined using \code{\link{clusterDataDefaults}}.  
  When no \code{defval} is given, an emtry string is the assumed value.
}

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}


\keyword{methods}

