\name{makeNodeAttrs}
\alias{makeNodeAttrs}
\title{make a list of character vectors that can be used as a value for
  the nodeAttrs argument in agopen}
\description{make a list of character vectors that can be used as a value for
  the nodeAttrs argument in agopen}
\usage{
makeNodeAttrs(g, label = nodes(g), shape = "ellipse",
              fillcolor = "#e0e0e0", ...)
}
\arguments{
  \item{g}{graph}
  \item{label}{character of length either \code{1} or
    \code{numnodes(g)}. If the length is \code{1}, the value is recycled.}
  \item{shape}{character of length either \code{1} or
    \code{numnodes(g)}}
  \item{fillcolor}{character of length either \code{1} or
    \code{numnodes(g)}}
  \item{...}{further named arguments that are character vectors of
length either 1 or numNodes(g)}
}
\details{This function is trivial but convenient.}

\value{A list of named character vectors, each of which with
length \code{numNodes(g)}.}

\author{Wolfgang Huber <huber@ebi.ac.uk>}
\examples{
g <- randomEGraph(letters[1:10], p=0.2)
makeNodeAttrs(g)
}
\keyword{dplot}
