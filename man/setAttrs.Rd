\name{setAttrsGraph}
\alias{LLsetAttrsGraph}
\alias{LLsetAttrsNode}
\alias{LLsetAttrsEdge}

\title{Set attributes of graph, nodes, edges for layout and/or rendering}
\description{Set attributes of graph, nodes, edges for layout and/or rendering}

\usage{
LLsetAttrsGraph(graph, attrname, attrval, defaultval="")
LLsetAttrsNode(graph, node, attrname, attrval, defaultval="")
LLsetAttrsEdge(graph, from, to, attrname, attrval, defaultval="")
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
Functions, \code{LLsetAttrsGraph}, \code{LLsetAttrsNode}, 
\code{LLsetAttrsEdge}, return TRUE if it successfully set attribute to the
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

attr_g = LLsetAttrsGraph(ag, attrname="bgcolor", attrval="red")
attr_n = LLsetAttrsNode(ag, "C", attrname="shape", attrval="tripleoctagon")
attr_e = LLsetAttrsEdge(ag, "A", "C", attrname="color", attrval="#40e0d0")

s_attr_e = LLsetAttrsEdge(ag, "A", "C", attrname="style", attrval="setlinewidth(14),dotted")


attr_n1 = LLsetAttrsNode(ag, "C", attrname=c("height","label", "fixedsize"), attrval=c("2", "test_C", "FALSE"))
attr_n2 = LLsetAttrsNode(ag, c("C","E", "D"), attrname=c("height","label", "fixedsize"), attrval=c("2", "test_C", "FALSE"))

}
\keyword{ models }

