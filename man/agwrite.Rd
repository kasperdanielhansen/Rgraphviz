\name{agwrite}
\alias{agwrite}
\alias{agread}
\title{A function to write a Ragraph object to a file}
\description{
  This function will take a \code{Ragraph} object and write it out in
  DOT format to a file.
}
\usage{
agwrite(graph, filename)
}
\arguments{
  \item{graph}{An object of class \code{Ragraph}}
  \item{filename}{ The output filename}
}
\details{
  This function is a wrapper to the agwrite() call in Graphviz.
}
\author{Jeff Gentry}
\seealso{\code{\link{agopen}}, \code{\link{agread}}}
\examples{
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, .2)
z <- agopen(g1,"foo",layout=FALSE)
agwrite(z,tempfile())
}
\keyword{graphs}
