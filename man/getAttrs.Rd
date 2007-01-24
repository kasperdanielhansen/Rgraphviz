\name{getAttrsGraph}
\alias{getAttrsGraph}
\alias{getAttrsNode}
\alias{getAttrsEdge}

\title{Get attributes of graph, nodes, edges for layout and/or rendering}
\description{Get attributes of graph, nodes, edges for layout and/or rendering}

\usage{
getAttrsGraph(graph, attrname)
getAttrsNode(graph, node, attrname)
getAttrsEdge(graph, from, to, attrname)
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
\code{getAttrsGraph} { returns attribute value(s). }
\code{getAttrsNode} { returns (node-name, attribute-value)-pair(s). }
\code{getAttrsEdge} { returns (edge-name, attribute-value)-pair(s). }
}

\references{Rgraphviz by E. Ganssner, S. North, www.graphviz.org }

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\examples{

library("Rgraphviz")
library("RBGL")

dd <- fromGXL(file(system.file("XML/dijkex.gxl",package="RBGL")))
ag <- agopen(dd, name="test")

attr_g = getAttrsGraph(ag, attrname="bgcolor")
attr_n = getAttrsNode(ag, "C", attrname="shape")
attr_e = getAttrsEdge(ag, "A", "C", attrname="color")

attr_n1 = getAttrsNode(ag, "C", attrname=c("height","label", "fixedsize"))
attr_n2 = getAttrsNode(ag, c("C","E", "D"), attrname=c("height","label", "fixedsize"))

}
\keyword{ models }

