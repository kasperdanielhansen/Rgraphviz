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
\code{plot(x, y, ..., nodeLabels, centerNode, nodeShape,
    defNodeCol=par("bg"), nodeCols=character(), defTextCol=par("fg"),
    textCols=character(), defEdgeCol=par("col"), edgeCols=list(),
    rankDir, fixedSize=TRUE, subGList)}
}
\section{arguments}{
  \item{x}{The \code{graph} object to plot}
  \item{y}{The layout method to use: One of \code{dot}, \code{neato}, and
    \code{twopi}.  The default is \code{dot}}
  \item{nodeLabels}{A character vector containing labels for the nodes.
    If not provided, the plot will use the node names.  If provided, the
    vector must have length equal to the number of nodes}
  \item{centerNode}{The node which is to be the center node in the
    plot.  Only useful with \code{twopi} layouts}
  \item{nodeShape}{The shape of the nodes, currently one of
    \code{circle} or \code{ellipse}, with \code{circle} being the
    default}
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
  \item{rankDir}{The direction that the plot is laid out in, one of
    either \code{TB} for Top-to-Bottom or \code{LR} for Left-to-Right.
    The default value is \code{TB}.  This argument is only useful for
    \code{dot} layouts.}
  \item{subGList}{A list of subgraphs taken from the primary
    \code{graph} object to be plotted.  If provided, these subgraphs
    will be clustered visually.  If not provided, no clusters will be used.}
  \item{...}{General commands to be sent to plot}
}
\section{details}{
  This method is currently only implemented for the \code{graphNEL}
  subclass of \code{graph}.
  
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
