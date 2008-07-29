\name{toFile}
\alias{toFile}

\title{Render a graph in a file with given format}
\description{Render a graph in a file with given format}

\usage{
toFile(graph,
          layoutType=c("dot","neato","twopi","circo","fdp"),
          filename,
          fileType=c("canon", "dot", "xdot", "dia", "fig",
                     "gd", "gd2", "gif", "hpgl", "imap", "cmapx",
                     "ismap", "mif", "mp", "pcl", "pdf", "pic",
                     "plain", "plain-ext", "png", "ps", "ps2",
                     "svg", "svgz", "vrml", "vtx", "wbmp"))
}

\arguments{
  \item{graph}{an instance of the \code{Ragraph} class}
  \item{layoutType}{which layout algorithm to use}
  \item{filename}{output file name}
  \item{fileType}{output file type}
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

\seealso{}

\examples{

library("graph")
library("Rgraphviz")

g1_gz <- gzfile(system.file("GXL/graphExample-01.gxl.gz",package="graph"), open="rb")
g1 <- fromGXL(g1_gz)
ag <- agopen(g1, name="test")

toFile(ag, layoutType="dot", filename="g1_dot.svg", fileType="svg")
toFile(ag, layoutType="neato", filename="g1_neato.ps", fileType="ps")
toFile(ag, layoutType="twopi", filename="g1_twopi.svg", fileType="svg")
toFile(ag, layoutType="circo", filename="g1_circo.png", fileType="png")

}
\keyword{ models }

