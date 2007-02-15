\name{edgeData-methods}
\docType{methods}
\alias{edgeData-methods}
\alias{edgeData<--methods}
\alias{edgeData}
\alias{edgeData<-}

\title{Get and set attributes for the edges of an Ragraph }

\usage{
edgeData(self, from, to, attr)
edgeData(self, from, to, attr, defval) <- value
}
\arguments{
  \item{self}{A \code{graph-class} instance}
  \item{from}{A \code{character} vector of from-nodes for the edges }
  \item{to}{A \code{character} vector of to-nodes for the edges }
  \item{attr}{A \code{character} vector of length one specifying the
    name of a edge attribute}
  \item{defval}{A \code{character} vector as the default attribute values}
  \item{value}{A \code{character} vector as the attribute values}
}

\description{
  Attributes of the edges of a graph can be accessed using \code{edgeData}.  
  The attributes must be defined using \code{\link{edgeDataDefaults}}.  
  When no \code{defval} is given, an empty string is the assumed value.
}

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\keyword{methods}

