\name{clusterAttrs}
\alias{LLgetDefAttrsCluster}
\alias{LLsetDefAttrsCluster}
\alias{LLgetAttrsCluster}
\alias{LLsetAttrsCluster}

\title{Handle attributes of clusters for layout and/or rendering}
\description{Handle attributes of clusters for layout and/or rendering}

\usage{
LLgetDefAttrsCluster(graph, cluster)
LLsetDefAttrsCluster(graph, cluster, attrnames=c(), attrvals=c())
LLgetAttrsCluster(graph, cluster, attrname)
LLsetAttrsCluster(graph, cluster, attrname, attrval, defaultval="")
}

\arguments{
  \item{graph}{an instance of the \code{Ragraph} class}
  \item{cluster}{number of the cluster}
  \item{attrnames}{attribute name(s)}
  \item{attrvals}{attribute value(s)}
  \item{attrname}{attribute name}
  \item{attrval}{attribute value}
  \item{defaultval}{default attribute value}
}
\details{
These functions mirror those for graph, node, edge.
TO FINALIZE THESE FUNCTIONS.
NEED MORE DETAILS.
}

\value{
\code{LLgetDefAttrsCluster} { returns attributes and their values}
\code{LLsetDefAttrsCluster} { returns NULL }
\code{LLgetAttrsCluster} { returns attribute value }
\code{LLsetAttrsCluster} { returns TRUE/FALSE }
}

\references{Rgraphviz by E. Ganssner, S. North, www.graphviz.org }

\author{Li Long <li.long@isb-sib.ch>}

\seealso{}

\examples{

library("Rgraphviz")
library("RBGL")

dd <- fromGXL(file(system.file("XML/dijkex.gxl",package="RBGL")))
ag <- agopen(dd, name="test")

# to add

}
\keyword{ models }

