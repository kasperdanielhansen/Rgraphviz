\name{plot-methods}
\docType{methods}
\alias{plot.graphNEL}
\alias{plot.graph}
\alias{drawEllipseNodes}
\alias{drawCircleNodes}
\alias{checkNodeShape}
\alias{checkCenterNode}
\alias{checkRankDir}
\alias{getCols}
\title{ Plot a graph object - methods}
\description{
  A plot method for \code{graph} objects.
}
\section{Usage}{
\code{plot(x, y, ..., nodeLabels, edgeLabels, nodeShape="circle",
    defNodeCol=par("bg"), nodeCols=character(), defTextCol=par("fg"),
    textCols=character(), defEdgeCol=par("col"), edgeCols=list(),
    fixedNodeSize=TRUE, subGList,attrs)}
}
\section{arguments}{
  \item{x}{The \code{graph} object to plot}
  \item{y}{The layout method to use: One of \code{dot}, \code{neato}, and
    \code{twopi}.  The default is \code{dot}}
  \item{nodeLabels}{A character vector containing labels for the nodes.
    If not provided, the plot will use the node names.  If provided, the
    vector must have length equal to the number of nodes}
  \item{edgeLabels}{A list providing labels for edges.  If not provided,
    no edge labels will be used.}
  \item{nodeShape}{The shape of the nodes.  Currently can be one of
    'circle' or 'ellipse'}
  \item{defNodeCol}{This is the default color to use for nodes}
  \item{nodeCols}{A named character vector of colors.  Any node which
    matches the name of an element in this vector will have that color
    used instead of \code{defNodeCol}}
  \item{defTextCol}{The default color to use for the node labels}
  \item{textCols}{A named character vector of colors.  Any node which
    matches the name of an element in this vector will have that color
    used for its label instead of \code{defTextCol}}
  \item{defEdgeCol}{The default color to use for plotting edges}
  \item{edgeCols}{A list of lists used to specify custom edge colors.
    Any element in this argument of the form node1$node2 in which there
    exists an edge between node1 and node2 will use node1$node2[[1]] as
    the color for that edge}
  \item{fixedNodeSize}{If TRUE, then all NODES will be forced to be
    of the same size (equal to the determined size of the largest node),
    otherwise Graphviz will size each node according to its own algorithms}
  \item{subGList}{A list of subgraphs taken from the primary
    \code{graph} object to be plotted.  If provided, these subgraphs
    will be clustered visually.  If not provided, no clusters will be used.}
  \item{attrs}{A list of Graphviz attributes to be sent to the layout engine}
  \item{...}{General commands to be sent to plot}
}
\section{details}{
  This method is currently only implemented for the \code{graphNEL}
  subclass of \code{graph}.

  The \code{edgeLabels} list format is such that each list element
  corresponds to a tail ('from') node.  Contained in that element will
  be a named vector, with the elements being the labels and the names of
  the elements corresponding to the head ('to') node.
  
  The \code{attrs} list requires a particular format.  It must be of
  length 3 with names \code{graph}, \code{node}, and \code{edge}.  Each
  of these elements themselves are lists - such that an element of
  \code{graph} corresponds to a graph element.  A full listing of
  attributes and their possible settings is available at
  http://www.research.att.com/~erg/graphviz/info/attrs.html.  All
  attribute values should be entered as character strings (even if the
  requested value is to be otherwise).
  
  Currently only global settings are implemented for nodes and edges,
  this will change in the near future such that these attributes can
  be set per edge/node.
  
  ****Details about things like the various layout methods, etc etc ****
}
\section{value}{
  A list containing the node locations, edge information and node
  labels.  The return value is invisible() however.
}
\section{author}{Jeff Gentry}
\section{seealso}{\code{\link{graphNEL-class}}, \code{\link{graph-class}},
  \code{\link{graph2graphviz}}}
\keyword{graphs}
\keyword{methods}
