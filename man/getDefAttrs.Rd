\name{getDefAttrsGraph}
\alias{LLgetDefAttrsGraph}
\alias{LLgetDefAttrsNode}
\alias{LLgetDefAttrsEdge}
\alias{LLgetDefAttrs}

\title{Get default attributes of graph, nodes, edges for layout and/or rendering}
\description{Get default attributes of graph, nodes, edges for layout and/or rendering}

\usage{
LLgetDefAttrsGraph(graph)
LLgetDefAttrsNode(graph)
LLgetDefAttrsEdge(graph)
LLgetDefAttrs(graph)
}

\arguments{
  \item{graph}{an instance of the \code{Ragraph} class}
}
\details{
Given a graph, returns the default attributes and their corresponding values 
for the graph, its nodes or its edges.
}

\value{
Functions, \code{LLgetDefAttrsGraph},  \code{LLgetDefAttrsNode},
\code{LLgetDefAttrsEdge}, return a nx2 matrix, each row gives a pair of
(attribute-name, attribute-value). 

Function \code{LLgetDefAttrs} returns the combined results from the other three.
}

\references{Rgraphviz by E. Ganssner, S. North, www.graphviz.org }

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\examples{

library("Rgraphviz")
library("RBGL")

dd <- fromGXL(file(system.file("XML/dijkex.gxl",package="RBGL")))
ag <- agopen(dd, name="test")

def_attr_g = LLgetDefAttrsGraph(ag)
def_attr_n = LLgetDefAttrsNode(ag)
def_attr_e = LLgetDefAttrsEdge(ag)
def_attr = LLgetDefAttrs(ag)

}
\keyword{ models }

