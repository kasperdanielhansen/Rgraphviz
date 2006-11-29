\name{getAttrsGraph}
\alias{LLgetAttrsGraph}
\alias{LLgetAttrsNode}
\alias{LLgetAttrsEdge}

\title{Get attributes of graph, nodes, edges for layout and/or rendering}
\description{Get attributes of graph, nodes, edges for layout and/or rendering}

\usage{
LLgetAttrsGraph(graph, attrname)
LLgetAttrsNode(graph, node, attrname)
LLgetAttrsEdge(graph, from, to, attrname)
}

\arguments{
  \item{graph}{an instance of the \code{Ragraph} class}
  \item{node}{name(s) of the node}
  \item{from}{name(s) of the from-node for the edge}
  \item{to}{name(s) of the to-node for the edge}
  \item{attrname}{attribute name(s)}
}
\details{
If more than one names are given for nodes and/or attributes, the normal R-rule
of circular rule applies.
}

\value{
\code{LLgetAttrsGraph} { returns attribute value(s). }
\code{LLgetAttrsNode} { returns (node-name, attribute-value)-pair(s). }
\code{LLgetAttrsEdge} { returns (edge-name, attribute-value)-pair(s). }
}

\references{Rgraphviz by E. Ganssner, S. North, www.graphviz.org }

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\examples{

library("Rgraphviz")
library("RBGL")

dd <- fromGXL(file(system.file("XML/dijkex.gxl",package="RBGL")))
ag <- agopen(dd, name="test")

attr_g = LLgetAttrsGraph(ag, attrname="bgcolor")
attr_n = LLgetAttrsNode(ag, "C", attrname="shape")
attr_e = LLgetAttrsEdge(ag, "A", "C", attrname="color")

attr_n1 = LLgetAttrsNode(ag, "C", attrname=c("height","label", "fixedsize"))
attr_n2 = LLgetAttrsNode(ag, c("C","E", "D"), attrname=c("height","label", "fixedsize"))

}
\keyword{ models }

