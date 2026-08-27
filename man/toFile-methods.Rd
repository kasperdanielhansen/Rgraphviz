\name{toFile-methods}
\docType{methods}
\alias{toFile}
\alias{toFile-methods}
\alias{toFile,character-method}
\alias{toFile,Ragraph-method}

\title{Render a graph in a file with given format}
\description{Render a graph in a file with given format}

\usage{
toFile(graph, layoutType = "dot", filename, fileType = "dot")
}

\arguments{
  \item{graph}{an instance of the \code{Ragraph} class; alternatively a
  character vector that represents the graph in \code{dot} format}
  \item{layoutType}{Which layout algorithm to use.  Defaults to dot, and
  see \code{graphvizCapabilities()$layoutTypes} for possible values.}
  \item{filename}{output file name}
  \item{fileType}{Output file type.  Defaults to \code{dot}, and
  see \code{graphvizCapabilities()$deviceTypes} for possible values.}
}
\details{
This function takes a given \code{Ragraph}, does the chosen layout, then 
renders the output to an external file.  Users could view the output file
with corresponding viewer.

The second usage of \code{toFile} is to use a character vector that
represents a graph in \code{dot} format. By using the character format,
all \emph{graphviz} options are possible in rendering the final
graph. However, please note that \emph{graphviz} as packaged in
\emph{Rgraphviz} currently has limited support for certain features
(e.g. HTML labels are not rendered). In this case, please consider
installing \emph{graphviz} on your computer, saving the dot graph
to a file and running it through the installed version to get improved
functionality (depending on your installation).
}

\value{
\code{toFile} returns NULL after writing to a file.
}

\references{Rgraphviz by E. Ganssner, S. North, www.graphviz.org }

\author{Li Long <li.long@isb-sib.ch>, Holger Hoefling <hhoeflin@gmail.com>}

\examples{
g1_gz <- gzfile(system.file("GXL/graphExample-01.gxl.gz",
                            package="graph"))

if(require(XML)) {
  g1 <- fromGXL(g1_gz)
  ag <- agopen(g1, name="test")

  toFile(ag, layoutType="dot", filename="g1_dot.svg", fileType="svg")
  toFile(ag, layoutType="neato", filename="g1_neato.ps", fileType="ps")
  toFile(ag, layoutType="twopi", filename="g1_twopi.svg", fileType="svg")
}

software.dot <- readLines(system.file("GV/softwareGraph.gv",
                                      package="Rgraphviz"))
toFile(software.dot, layoutType="dot", filename="softwareGraph.svg", fileType="svg")
}
\keyword{methods}
\keyword{graphs}

