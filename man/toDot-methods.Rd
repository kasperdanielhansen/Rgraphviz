\name{toDot-methods}
\docType{methods}
\alias{toDot}
\alias{toDot-methods}
\alias{toDot,graph-method}
\title{A Generic For Converting Objects To Dot}
\description{
  This generic is used to convert objects of varying classes to the Dot
  language.  Currently, only the \code{graph} class is supported.
}
\usage{
  toDot(graph, filename, ...)
}
\arguments{
  \item{graph}{The \code{graph} to output to Dot}
  \item{filename}{The name of the file to output to.}
  \item{\dots}{Any arguments to pass on to \code{\link{agopen}}}
}
\section{details}{
  The method defined for \code{graph} objects is a convenience wrapper
  around \code{\link{agopen}} and \code{\link{agwrite}} in that order.
  It will take an object of class \code{graph} (or one of its
  subclasses), call \code{agopen} (any extra arguments besides the graph
  and the \code{name} parameter should be passed in via \code{\dots})
  and then write the resulting information via \code{\link{agwrite}} in
  the file specified by \code{filename}.
}
\section{author}{Jeff Gentry}
\section{seealso}{\code{\link{agopen}}, \code{\link{agwrite}},
  \code{\link[graph]{graph-class}}}
\examples{
  set.seed(123)
  V <- letters[1:10]
  M <- 1:4
  g1 <- randomGraph(V, M, .2)

  nAttrs <- list()
  eAttrs <- list()
  nAttrs$label <- c(a="lab1", b="lab2", g="lab3", d="lab4")
  eAttrs$label <- c("a~h"="test", "c~h"="test2")
  nAttrs$color <- c(a="red", b="red", g="green", d="blue")
  eAttrs$color <- c("a~d"="blue", "c~h"="purple")

  toDot(g1, tempfile(), nodeAttrs=nAttrs, edgeAttrs=eAttrs)
}
\keyword{methods}
\keyword{graphs}

