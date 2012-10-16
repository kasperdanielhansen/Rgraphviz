\name{agwrite}
\alias{agwrite}
\alias{agread}
\title{Read / write Ragraph objects}
\description{
  These functions will write an \code{Ragraph} object to or from a file.
}
\usage{
agwrite(graph, filename)
agread(filename, layoutType="dot", layout=TRUE)
}
\arguments{
  \item{graph}{An object of class \code{Ragraph}}
  \item{filename}{The input or output filename}
  \item{layoutType}{character(1) specifying the format of the input
    file. must be one of \code{graphvizCapabilities()$layoutTypes}.}
  \item{layout}{logical(1) indicating whether \code{\link{graphLayout}}
    is to be called on the result of file input.}
}
\details{
  These function are wrappers to agwrite() and agread() calls in Graphviz.
}
\author{Jeff Gentry}
\seealso{\code{\link{agopen}}, \code{\link{agread}}}
\examples{
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, .2)
z <- agopen(g1, "foo", layout=FALSE) ## default layoutType 'dot'
file <- tempfile()
agwrite(z, file)


## supported input types
graphvizCapabilities()$layoutTypes
g2 <- agread(file, layout=TRUE)
if (interactive())
   plot(g2)
}
\keyword{graphs}
