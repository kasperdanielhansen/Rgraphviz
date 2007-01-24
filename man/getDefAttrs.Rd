\name{getDefAttrsGraph}
\alias{getDefAttrsGraph}
\alias{getDefAttrsNode}
\alias{getDefAttrsEdge}
\alias{getDefAttrs}

\title{Get default attributes of graph, nodes, edges for layout and/or rendering}
\description{Get default attributes of graph, nodes, edges for layout and/or rendering}

\usage{
getDefAttrsGraph(graph)
getDefAttrsNode(graph)
getDefAttrsEdge(graph)
getDefAttrs(graph)
}

\arguments{
  \item{graph}{an instance of the \code{Ragraph} class}
}
\details{
Given a graph, returns the default attributes and their corresponding values 
for the graph, its nodes or its edges.
}

\value{
Functions, \code{getDefAttrsGraph},  \code{getDefAttrsNode},
\code{getDefAttrsEdge}, return a nx2 matrix, each row gives a pair of
(attribute-name, attribute-value). 

Function \code{getDefAttrs} returns the combined results from the other three.
}

\references{Rgraphviz by E. Ganssner, S. North, www.graphviz.org }

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\examples{

library("Rgraphviz")
library("RBGL")

dd <- fromGXL(file(system.file("XML/dijkex.gxl",package="RBGL")))
ag <- agopen(dd, name="test")

def_attr_g = getDefAttrsGraph(ag)
def_attr_n = getDefAttrsNode(ag)
def_attr_e = getDefAttrsEdge(ag)
def_attr = getDefAttrs(ag)

}
\keyword{ models }

