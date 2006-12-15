\name{setDefAttrsGraph}
\alias{LLsetDefAttrsGraph}
\alias{LLsetDefAttrsNode}
\alias{LLsetDefAttrsEdge}
\alias{LLsetDefAttrs}

\title{Get default attributes of graph, nodes, edges for layout and/or rendering}
\description{Get default attributes of graph, nodes, edges for layout and/or rendering}

\usage{
LLsetDefAttrsGraph()
LLsetDefAttrsNode()
LLsetDefAttrsEdge()
LLsetDefAttrs()
}

\arguments{
}
\details{
Given a graph, returns the default attributes and their corresponding values 
for the graph, its nodes or its edges.
}

\value{
Functions, \code{LLsetDefAttrsGraph},  \code{LLsetDefAttrsNode},
\code{LLsetDefAttrsEdge}, return a nx2 matrix, each row gives a pair of
(attribute-name, attribute-value). 

Function \code{LLsetDefAttrs} returns the combined results from the other three.
}

\references{Rgraphviz by E. Ganssner, S. North, www.graphviz.org }

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\examples{

library("Rgraphviz")
library("RBGL")

dd <- fromGXL(file(system.file("XML/dijkex.gxl",package="RBGL")))
ag <- agopen(dd, name="test")

def_attr_g = LLsetDefAttrsGraph()
def_attr_n = LLsetDefAttrsNode()
def_attr_e = LLsetDefAttrsEdge()
def_attr = LLsetDefAttrs()

}
\keyword{ models }

