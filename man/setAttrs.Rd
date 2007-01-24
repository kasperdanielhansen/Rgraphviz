\name{setAttrsGraph}
\alias{setAttrsGraph}
\alias{setAttrsNode}
\alias{setAttrsEdge}

\title{Set attributes of graph, nodes, edges for layout and/or rendering}
\description{Set attributes of graph, nodes, edges for layout and/or rendering}

\usage{
setAttrsGraph(graph, attrname, attrval, defaultval="")
setAttrsNode(graph, node, attrname, attrval, defaultval="")
setAttrsEdge(graph, from, to, attrname, attrval, defaultval="")
}

\arguments{
  \item{graph}{an instance of the \code{Ragraph} class}
  \item{node}{name(s) of the node}
  \item{from}{name(s) of the from-node for the edge}
  \item{to}{name(s) of the to-node for the edge}
  \item{attrname}{attribute name(s)}
  \item{attrval}{attribute value(s) to be set to}
  \item{defaultval}{default attribute value(s) if the attribute(s) are new}
}
\details{
If more than one names are given for nodes and/or attributes, the normal R-rule
of circular rule applies.

These functions set the given attribute(s) to the new value(s) for the given 
graph, node(s) or edge(s).
}

\value{
Functions, \code{setAttrsGraph}, \code{setAttrsNode}, 
\code{setAttrsEdge}, return TRUE if it successfully set attribute to the
new value, return FALSE otherwise.
}

\references{Rgraphviz by E. Ganssner, S. North, www.graphviz.org }

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\examples{

library("Rgraphviz")
library("RBGL")

dd <- fromGXL(file(system.file("XML/dijkex.gxl",package="RBGL")))
ag <- agopen(dd, name="test")

attr_g = setAttrsGraph(ag, attrname="bgcolor", attrval="red")
attr_n = setAttrsNode(ag, "C", attrname="shape", attrval="tripleoctagon")
attr_e = setAttrsEdge(ag, "A", "C", attrname="color", attrval="#40e0d0")

s_attr_e = setAttrsEdge(ag, "A", "C", attrname="style", attrval="setlinewidth(14),dotted")


attr_n1 = setAttrsNode(ag, "C", attrname=c("height","label", "fixedsize"), attrval=c("2", "test_C", "FALSE"))
attr_n2 = setAttrsNode(ag, c("C","E", "D"), attrname=c("height","label", "fixedsize"), attrval=c("2", "test_C", "FALSE"))

}
\keyword{ models }

