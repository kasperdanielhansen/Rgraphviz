\name{toFile}
\alias{LLtoFile}

\title{Render a graph in a file with given format}
\description{Render a graph in a file with given format}

\usage{
LLtoFile(graph,
          layoutType=c("dot","neato","twopi","circo","fdp"),
          filename,
          fileType=c("canon", "dot", "xdot", "dia", "fig",
                     "gd", "gd2", "gif", "hpgl", "imap", "cmapx",
                     "ismap", "mif", "mp", "pcl", "pic",
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
\code{LLtoFile} returns NULL after writing to a file.
}

\references{Rgraphviz by E. Ganssner, S. North, www.graphviz.org }

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\examples{

library("Rgraphviz")
library("RBGL")

dd <- fromGXL(file(system.file("XML/dijkex.gxl",package="RBGL")))
ag <- agopen(dd, name="test")

LLtoFile(ag, layoutType="dot", filename="test2.svg", fileType="svg")
LLtoFile(ag, layoutType="neato", filename="test2.ps", fileType="ps")
LLtoFile(ag, layoutType="twopi", filename="test3.svg", fileType="svg")
LLtoFile(ag, layoutType="circo", filename="test3.png", fileType="png")

}
\keyword{ models }

