\name{toFile}
\alias{toFile}

\title{Render a graph in a file with given format}
\description{Render a graph in a file with given format}

\usage{
toFile(graph, layoutType = "dot", filename, fileType = "dot")
}

\arguments{
  \item{graph}{an instance of the \code{Ragraph} class}
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
}

\value{
\code{toFile} returns NULL after writing to a file.
}

\references{Rgraphviz by E. Ganssner, S. North, www.graphviz.org }

\author{Li Long <li.long@isb-sib.ch>}

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
}
\keyword{ models }

