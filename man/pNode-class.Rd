\name{pNode-class}
\docType{class}
\alias{pNode-class}
\alias{pNode}
\alias{name,pNode-method}
\title{Class "pNode": A class to plot nodes}
\description{This class is used to transfer information to Graphviz
  that is necessary to represent and plot a node.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pNode", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}: The name of
      the node, used to reference the node.}
    \item{\code{attrs}:}{Object of class \code{"list"}: A list of
      attributes specific to this node.}
    \item{\code{subG}:}{Object of class \code{"integer"}: Which subgraph
      this node is a part of.} 
}
}
\section{Methods}{
  \describe{
    \item{name}{\code{signature(object = "pNode")}: Retrieves the
      \code{name} slot of the object.}
  }
}
\details{
  The \code{attrs} slot is a named list, where the names correspond to
  attributes and the values in the list correspond to the value for that
  element's attribute.

  The \code{subG} slot describes which subgraph this node is a part of.
  A value of \code{0} implies that the node is not a member of any subgraph.
}
\author{R. Gentleman and Jeff Gentry}
\seealso{\code{\link{pEdge}}, \code{\link{agopen}}, \code{\link{buildNodeList}}}
\examples{
   set.seed(123)
   V <- letters[1:10]
   M <- 1:4
   g1 <- randomGraph(V, M, .2)

   z <- buildNodeList(g1)
   z[[1]]   ## Object of type pNode
}
\keyword{classes}
