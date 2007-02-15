\name{nodeDataDefaults-methods}
\docType{methods}
\alias{nodeDataDefaults-methods}
\alias{nodeDataDefaults<--methods}
\alias{nodeDataDefaults}
\alias{nodeDataDefaults<-}

\title{Get and set default attributes for the nodes of an Ragraph}

\usage{
nodeDataDefaults(self)
nodeDataDefaults(self, attr) <- value
}
\arguments{
  \item{self}{A \code{graph-class} instance}
  \item{attr}{A \code{character} vector of length one giving the name of
  an attribute}
  \item{value}{A \code{character} vector to set as the default values }
}
\description{
  You can associate various attributes with the nodes of a graph.  Use
  \code{nodeDataDefaults} to specify the set of attributes that describe
  nodes.  Each attribute must have a default value.  You can set the
  attribute for a particular node or set of nodes using
  \code{\link{nodeData}}.
}

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\keyword{methods}

