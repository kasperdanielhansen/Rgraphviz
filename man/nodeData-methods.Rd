\name{nodeData-methods}
\docType{methods}
\alias{nodeData-methods}
\alias{nodeData<--methods}
\alias{nodeData}
\alias{nodeData<-}

\title{Get and set attributes for the nodes of an Ragraph }

\usage{
nodeData(self, n, attr)
nodeData(self, n, attr, defval) <- value
}
\arguments{
  \item{self}{A \code{graph-class} instance}
  \item{n}{A \code{character} vector of node names}
  \item{attr}{A \code{character} vector of length one specifying the
    name of a node attribute}
  \item{defval}{A \code{character} vector as the default attribute values}
  \item{value}{A \code{character} vector as the attribute values}
}

\description{
  Attributes of the nodes of a graph can be accessed using \code{nodeData}.  
  The attributes must be defined using \code{\link{nodeDataDefaults}}.  
  When no \code{defval} is given, an empty string is the assumed value.
}

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\keyword{methods}

