\name{plot-methods}
\docType{methods}
\alias{plot.graphNEL}
\alias{plot.graph}
\alias{drawCircleNodes}
\title{ Plot a graph object - methods}
\description{
  A plot method for \code{graph} objects.
}
\section{Usage}{
  \code{plot(x, y, ..., subGList=list(), attrs=getDefaultAttrs(y),
        nodeAttrs=list(), edgeAttrs=list(), xlab="", ylab="")}
}
\section{arguments}{
  \item{x}{The \code{graph} object to plot}
  \item{y}{The layout method to use: One of \code{dot}, \code{neato}, and
    \code{twopi}.  The default is \code{dot}}
  \item{subGList}{A list of subgraphs taken from the primary
    \code{graph} object to be plotted.  If provided, these subgraphs
    will be clustered visually.  If not provided, no clusters will be used.}
  \item{attrs}{A list of Graphviz attributes to be sent to the layout engine}
  \item{nodeAttrs}{A list of attributes for specific nodes}
  \item{edgeAttrs}{A list of attributes for specific edges}
  \item{xlab}{Label for the x axis of the plot}
  \item{ylab}{Label for the y axis of the plot}
  \item{...}{General commands to be sent to plot}
}
\section{details}{
  This method is currently only implemented for the \code{graphNEL}
  subclass of \code{graph}.
  
  The \code{attrs} list requires a particular format.  It must be of
  length 3 with names \code{graph}, \code{node}, and \code{edge}.  Each
  of these elements themselves are lists - such that an element of
  \code{graph} corresponds to a graph element.  A full listing of
  attributes and their possible settings is available at
  http://www.research.att.com/~erg/graphviz/info/attrs.html.  All
  attribute values should be entered as character strings (even if the
  requested value is to be otherwise).

  The \code{nodeAttrs} list is used to specify attributes on a
  particular node, instead of for all nodes.  The format of this list is
  such that the elements correspond to attributes (the name of the
  element is used to note which attribute) and each element contains a
  named vector.  The names of the vector denote which nodes are having
  this attribute set and the values in the vector specify the value.

  The \code{edgeAttrs} list is identical in format to \code{nodeAttrs}.
  However, the name of the edges is in a particular format where an edge
  between x and y is named \code{x~y}.  Note that even in an undirected
  graph that \code{x~y} is not the same as \code{y~x} - the name must be
  in the same order that the edge was defined as having.  
}
\section{value}{
  The \code{Ragraph} object used for plotting
}
\section{author}{Jeff Gentry}
\section{seealso}{\code{\link{graphNEL-class}}, \code{\link{graph-class}},
  \code{\link{graph2graphviz}}}
\examples{
#  WHY DOES THIS NOT WORK IN CHECK?
#  V <- letters[1:10]
#  M <- 1:4
#  g1 <- randomGraph(V, M, .2)
#  plot(g1)
}
\keyword{graphs}
\keyword{methods}
