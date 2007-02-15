\name{graphData-methods}
\docType{methods}
\alias{graphData-methods}
\alias{graphData<--methods}
\alias{graphData}
\alias{graphData<-}

\title{Get and set attributes of an Ragraph object}

\usage{
graphData(self, attr)
graphData(self, attr, defval) <- value
}
\arguments{
  \item{self}{A \code{Ragraph-class} instance}
  \item{attr}{A \code{character} vector of length one specifying the
  name of a graph attribute}
  \item{defval}{A \code{character} vector to store as the default attribute values}
  \item{value}{A \code{character} vector to store as the attribute values}
}

\description{
  Attributes of a graph can be accessed using \code{graphData}.  
  The attributes could be defined using \code{\link{graphDataDefaults}}.  
  When no \code{defval} is given, an empty string is the assumed value.
}

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}


\keyword{methods}

