\name{GraphvizLayouts}
\alias{GraphvizLayouts}
\alias{dot}
\alias{twopi}
\alias{neato}
\title{Graphviz Layout Methods}
\description{
  The following describes the different layout methods that can be used
  within Rgraphviz.  Each layout method has its own particular
  advantages and disadvantages and can have its own quirks.  Currently
  Rgraphviz supports three different layout methods: \code{dot},
  \code{twopi} and \code{neato}.
}
\section{The dot layout}{
  The \code{dot} algorithm produces a ranked layout of a graph
  honoring edge directions. It is particularly appropriate for displaying
  hierarchies or directed acyclic graphs. The basic layout scheme is
  attributed to Sugiyama et al. The specific algorithm used by
  dot follows the steps described by Gansner et al.
  
  \code{dot} draws a graph in four main phases. Knowing this helps you to
  understand what kind of layouts dot makes and how you can control
  them.  The layout procedure used by dot relies on the graph being
  acyclic. Thus, the first step is to break any cycles which occur in
  the input graph by reversing the internal direction of certain
  cyclic edges. The next step assigns nodes to discrete ranks or
  levels. In a top-to-bottom drawing, ranks determine Y
  coordinates. Edges that span more than one rank are broken into
  chains of \code{virtual} nodes and unit-length edges. The third step
  orders nodes within ranks to avoid crossings. The fourth step sets X
  coordnates of nodes to keep edges short, and the final step routes
  edge splines.
  
  In \code{dot}, higher edge weights have the effect of causing edges
  to be shorter and straighter.
  
  Fine-tuning should be approached cautiously. \code{dot} works best when it can
  makes a layout without much \code{help} or interference in its
  placement of individual nodes and edges. Layouts can be adjusted
  somewhat by increasing the weight of certain edges, and sometimes
  even by rearranging the order of nodes and edges in the file. But
  this can backfire because the layouts are not necessarily stable
  with respect to changes in the input graph. One last adjustment can
  invalidate all previous changes and make a very bad drawing.
}
\section{The neato layout}{
  \code{neato} is a program that makes layouts of undirected graphs
  following the filter model of \code{dot}. Its layout heuristic
  creates virtual physical models and runs an iterative solver to find
  low energy configurations.  An ideal spring is placed between every
  pair of nodes such that its length is set to the shortest path distance
  between the endpoints. The springs push the nodes so their geometric
  distance in the layout approximates their path distance in the
  graph.

  In \code{neato}, the edge weight is the strength of the
  corresponding spring.
  
  As with \code{dot}, fine-tuning should be approached cautiously, as
  often small changes can have a drastic effect and create a poor
  looking layout.
}
\section{The twopi layout}{
  The radial layout algorithm represented by \code{twopi} is conceptually the
  simplest. It takes a node specified as the center of the
  layout and the root of the generated spanning tree. The remaining
  nodes are placed on a series of concentric circles about the center,
  the circle used corresponding to the graph-theoretic distance from
  the node to the center. Thus, for example, all of the neighbors of
  the center node are placed on the first circle around the
  center. The algorithm allocates angular slices to each branch of the
  induced spanning tree to guarantee enough space for the tree on each
  ring.  It should be obvious from the description that the basic
  version of the twopi algorithm relies on the graph being
  connected.
  
  Of great importance to the quality of the layout is the selection of
  an appropriate center node.  By default, the \code{twopi} will
  randomly pick one of the nodes that are furthest from a leaf node,
  where a leaf node is a node of degree \code{1}.  The \code{root} attribute
  can be used to manually select a central node for the layout, and
  users are encouraged to use this attribute to select a node which
  provides a good quality layout.  It often might not be obvious what
  that node will be, as it will vary from graph to graph, so some
  experimentation might be required.
 
  As with \code{dot} and \code{neato}, fine-tuning should be approached
  cautiously, as often small changes can have a drastic effect and create a poor
  looking layout.  The \code{root} node of the layout, as mentioned
  before, can have a profound effect on the outcome of the layout and
  care should be taken to select an appropriate one.
}

\author{Jeff Gentry}
\references{\url{http://www.research.att.com/sw/tools/graphviz/dotguide.pdf},
  \url{http://www.research.att.com/sw/tools/graphviz/neatoguide.pdf},
  \url{http://www.research.att.com/sw/tools/graphviz/libguide.pdf}}
\seealso{\code{\link{GraphvizAttributes}}, \code{\link{plot.graph}},
  \code{\link{agopen}}}
\examples{
}
\keyword{graphs}
