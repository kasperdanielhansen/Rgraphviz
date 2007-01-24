\name{setDefAttrs}
\alias{setDefAttrsGraph}
\alias{setDefAttrsNode}
\alias{setDefAttrsEdge}
\alias{setDefAttrs}

\title{Set default attributes of graph, nodes, edges for layout and/or rendering}
\description{Set default attributes of graph, nodes, edges for layout and/or rendering}

\usage{
setDefAttrsGraph(graph, attrnames=c(), attrvals=c())
setDefAttrsNode(graph, attrnames=c(), attrvals=c())
setDefAttrsEdge(graph, attrnames=c(), attrvals=c())
setDefAttrs(graph, g_attrnames=c(), g_attrvals=c(), n_attrnames=c(), n_attrvals=c(), e_attrnames=c(), e_attrvals=c())
}

\arguments{
\item{graph} {An object of class \code{Ragraph}}
\item{attrnames} {names of attributes }
\item{attrvals} {corresponding default values for attributes}
\item{g_attrnames} {names of attributes for graph}
\item{g_attrvals} {corresponding default values for attributes for graph}
\item{n_attrnames} {names of attributes for nodes}
\item{n_attrvals} {corresponding default values for attributes for nodes}
\item{e_attrnames} {names of attributes for edges}
\item{e_attrvals} {corresponding default values for attributes for edges}
}

\details{
Sets the default attributes to their corresponding values for the graph, its nodes or its edges.
}

\value{
Functions, \code{setDefAttrsGraph},  \code{setDefAttrsNode},
\code{setDefAttrsEdge}, set default attributes for graph, nodes, edges.  
Some attributes could affect layout, others affect render only.  
Different layout engines and different renderer could use different attributes.

Function \code{setDefAttrs} sets attributes for graph, nodes and edges.

Notice: each attribute can be set to default value once and only once.  
Subsequent calls to set it have no effect.
}

\references{Rgraphviz by E. Ganssner, S. North, www.graphviz.org }

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\examples{

library("Rgraphviz")
library("RBGL")

dd <- fromGXL(file(system.file("XML/dijkex.gxl",package="RBGL")))
ag <- LLagopen(dd, name="test")

setDefAttrsGraph(ag, attrnames=c("fontcolor"), attrvals=c("green"))
setDefAttrsNode(ag, attrnames=c("fillcolor", "shape"), attrvals=c("red", "rect"))
setDefAttrsEdge(ag, attrnames=c("color"), attrvals=c("blue"))

# this is equivalent to the above 3 calls
setDefAttrs(ag, g_attrnames=c("fontcolor"), g_attrvals=c("green"),
	n_attrnames=c("fillcolor", "shape"), n_attrvals=c("red", "rect"),
	e_attrnames=c("color"), e_attrvals=c("blue"))

}
\keyword{ models }

