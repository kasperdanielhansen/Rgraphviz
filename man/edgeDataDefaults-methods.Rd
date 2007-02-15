\name{edgeDataDefaults-methods}
\docType{methods}
\alias{edgeDataDefaults-methods}
\alias{edgeDataDefaults<--methods}
\alias{edgeDataDefaults}
\alias{edgeDataDefaults<-}

\title{Get and set default attributes for the edges of an Ragraph}

\usage{
edgeDataDefaults(self)
edgeDataDefaults(self, attr) <- value
}
\arguments{
  \item{self}{A \code{graph-class} instance}
  \item{attr}{A \code{character} vector of length one giving the name of
  an attribute}
  \item{value}{A \code{character} vector to set as the default values }
}
\description{
  You can associate various attributes with the edges of a graph.  Use
  \code{edgeDataDefaults} to specify the set of attributes that describe
  edges.  Each attribute must have a default value.  You can set the
  attribute for a particular edge or set of edges using
  \code{\link{edgeData}}.
}

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\keyword{methods}

