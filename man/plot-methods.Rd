\name{plot-methods}
\docType{methods}
\alias{plot.graphNEL}
\alias{plot.graph}
\alias{plot.Ragraph}
\alias{drawCircleNodes}
\title{ Plot a graph object - methods}
\description{
  A plot method for \code{graph} objects.
}
\section{Usage}{
  \code{'graph': plot(x, y, ..., subGList=list(), attrs=getDefaultAttrs(y),
    nodeAttrs=list(), edgeAttrs=list(), xlab="", ylab="", main=NULL, sub=sub)}

  \code{'Ragraph': plot(x, y, ..., attrs, xlab="", ylab="", main=NULL,
    sub=sub, drawNode=drawAgNode, newPlot=TRUE)}
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
  \item{main}{Main label for the plot}
  \item{sub}{Subtitle for the plot}
  \item{drawNode}{Function to draw the nodes.  The default is
    \code{\link{drawAgNode}}}
  \item{newPlot}{Whether or not this is a new plot.  When
    \code{plot.Ragraph} is called from \code{plot.graph}, the plotting
    device has already been setup and should not be reinitialized.
    However, when \code{plot.Ragraph} is being called directly this is
    not the case}.
  \item{...}{General commands to be sent to plot}
}
\section{details}{
  The first plot method in the \code{usage} section corresponds to the
  \code{graph} class in the \code{graph} package.  It will convert the
  \code{graph} object into an object of class \code{Ragraph} by using
  Graphviz to perform the layout.  Then it will call the plot method for
  \code{Ragraph}.  The \code{plot.graph} method is nothing more then a
  wrapper that calls \code{\link{agopen}} and \code{plot.Ragraph}.

  The second plot method in the \code{usage} section is for the
  \code{Ragraph} class, which describes a Graphviz structure of a
  graph.  This method will extract necessary information from the object
  and use it to plot the graph.

  Users can specify arbitrary drawing functions for the nodes of the
  \code{Ragraph} with the \code{drawNode} argument, although caution is
  urged.  The default drawing function for 
  all nodes is \code{\link{drawAgNode}}, which will draw a basic circle,
  ellipse or rectangle according to the layout specifications for each
  node. If supplying a custom function, users are encouraged to look at
  the code of this function for a more clear picture of the information
  required to properly draw a node.   Users can specify either one
  custom function to be used for all nodes or a list (where length is
  equal to the nubmer of nodes) of functions where the Nth element in
  the list provides the drawing function for the Nth node, and every
  function will take two parameters - the first is an object of class
  \code{AgNode} representing the node itself and the second is an object
  of class \code{\link{xyPoint}} representing the upper right corner of
  the Graphviz plotting region (where the lower left is \code{0,0}).
  The second parameter will not be necessary for most users, but can be
  useful if it is necessary to map the Graphviz points to inch values
  (see \code{\link{drawCircleNode}} for an example of a function that
  needs this.)
  
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
