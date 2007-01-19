\name{Ragraph-class}
\docType{class}
\alias{Ragraph-class}
\alias{Ragraph}
\alias{agraph}
\alias{laidout}
\alias{layoutType}
\alias{edgemode}
\alias{boundBox}
\alias{show,Ragraph-method}
\alias{edgeNames,Ragraph-method}

\alias{graphDataDefaults,Ragraph-method}
\alias{graphDataDefaults<-,Ragraph,vector,vector-method}
\alias{graphData<-,Ragraph,character,character-method}
\alias{graphData,Ragraph,character-method}
\alias{clusterDataDefaults,Ragraph,numeric-method}
\alias{clusterDataDefaults<-,Ragraph,numeric,vector,vector-method}
\alias{clusterData,Ragraph,numeric,character-method}
\alias{clusterData<-,Ragraph,numeric,character,character-method}
\alias{edgeDataDefaults,Ragraph,missing-method}
\alias{edgeDataDefaults<-,Ragraph,vector,vector-method}
\alias{edgeData<-,Ragraph,character,character,character,character-method}
\alias{edgeData,Ragraph,character,character,character-method}
\alias{nodeDataDefaults,Ragraph,missing-method}
\alias{nodeDataDefaults<-,Ragraph,vector,vector-method}
\alias{nodeData,Ragraph,character,character-method}
\alias{nodeData<-,Ragraph,character,character,character-method}

\alias{AgEdge,Ragraph-method}
\alias{AgEdge<-,Ragraph-method}
\alias{agraph,Ragraph-method}
\alias{boundBox,Ragraph-method}
\alias{laidout,Ragraph-method}
\alias{layoutType,Ragraph-method}
\alias{edgemode,Ragraph-method}
\alias{AgNode,Ragraph-method}
\alias{AgNode<-,Ragraph-method}
\alias{getNodeLW,Ragraph-method}
\alias{getNodeRW,Ragraph-method}
\alias{getNodeHeight,Ragraph-method}
\alias{getNodeXY,Ragraph-method}
\alias{getNodeXY}
\alias{getNodeNames}
\alias{getNodeLabels}
\title{Class "Ragraph": A class to handle libgraph representations}
\description{Class \code{Ragraph} is used to handle libgraph
  representations of R graph objects.}
\section{Objects from the Class}{
Objects can be created by calls to the function \code{agopen}.
}
\section{Slots}{
  \describe{
    \item{\code{agraph}:}{Object of class \code{"externalptr"}: A C
      based structure containing the libgraph information}
    \item{\code{laidout}:}{Object of class \code{"logical"}: Whether or
      not this graph has been laid out or not.}
    \item{\code{layoutType}:}{Object of class \code{"character"}: The
      layout method used for this object}
    \item{\code{edgemode}:}{Object of class \code{"character"}: The
      edgemode for this graph - \dQuote{directed} or \dQuote{undirected}}
    \item{\code{AgNode}:}{Object of class \code{"list"}: A list of
      \code{AgNode} objects.}
    \item{\code{AgEdge}:}{Object of class \code{"list"}: A list of
      \code{AgEdge} objects.}
    \item{\code{boundBox}:}{Object of class \code{"boundBox"}: Defines
      the bounding box of the plot.}
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Ragraph")}: A brief summary of
      the contents}
    \item{agraph}{\code{signature(object = "Ragraph")}: Returns the
      external libgraph pointer}
    \item{laidout}{\code{signature(object = "Ragraph")}: Returns the
      \code{laidout} slot}
    \item{boundBox}{\code{signature(object = "Ragraph")}: Returns the
      bounding box.}
    \item{AgEdge}{\code{signature(object = "Ragraph")}: Returns the edge
      list.}
    \item{AgNode}{\code{signature(object = "Ragraph")}: Returns the node
      list.}
    \item{edgemode}{\code{signature(object = "Ragraph")}: Retrieves the
      edgemode of this object.}
    \item{layoutType}{\code{signature(object = "Ragraph")}: Retrieves the
      method used for the layout of this graph.}
    \item{edgeNames}{\code{signature(object = "Ragraph")}: Returns a
      vector of the names of the edges in this graph.}

    \item{graphDataDefaults}{\code{signature(self= "Ragraph")}: 
      Gets default attributes of the given graph.}
    \item{graphDataDefaults<-}{\code{signature(self= "Ragraph", attr="vector", value="vector")}: 
      Sets default attributes of the given graph.}
    \item{graphData}{\code{signature(self= "Ragraph", attr="character")}: 
      Gets attributes of the given graph.}
    \item{graphData<-}{\code{signature(self= "Ragraph", attr="character", value="character")}: 
      Sets attributes of the given graph.}

    \item{clusterDataDefaults}{\code{signature(self= "Ragraph", cluster="numeric")}: 
      Gets default attributes of a cluster for the given graph.}
    \item{clusterDataDefaults<-}{\code{signature(self= "Ragraph", cluster="numeric", attr="vector", value="vector")}: 
      Sets default attributes of a cluster for the given graph.}
    \item{clusterData}{\code{signature(self= "Ragraph", cluster="numeric", attr="character")}: 
      Gets attributes of a cluster for the given graph.}
    \item{clusterData<-}{\code{signature(self= "Ragraph", cluster="numeric", attr="character", value="character")}: 
      Sets attributes of a cluster for the given graph.}

    \item{edgeDataDefaults}{\code{signature(self= "Ragraph", attr="missing")}: 
      Gets default attributes of the given edge.}
    \item{edgeDataDefaults<-}{\code{signature(self= "Ragraph", attr="vector", value="vector")}: 
      Sets default attributes of the given edge.}
    \item{edgeData}{\code{signature(self= "Ragraph", from="character", to="character", attr="character")}: 
      Gets attributes of the given edge.}
    \item{edgeData<-}{\code{signature(self= "Ragraph", from="character", to="character", attr="character", value="character")}: 
      Sets attributes of the given edge.}

    \item{nodeDataDefaults}{\code{signature(self= "Ragraph", attr="missing")}: 
      Gets default attributes of the given node.}
    \item{nodeDataDefaults<-}{\code{signature(self= "Ragraph", attr="vector", value="vector")}: 
      Sets default attributes of the given node.}
    \item{nodeData}{\code{signature(self= "Ragraph", n="character", attr="character")}: 
      Gets attributes of the given node.}
    \item{nodeData<-}{\code{signature(self= "Ragraph", n="character", attr="character", value="character")}: 
      Sets attributes of the given node.}

    \item{getNodeXY}{\code{signature(object = "Ragraph")}: Returns a two
      element list, the first element contains a numerical vector with
      the 'x' positions of every node in this graph, and the second
      element contains a numerical vector with the 'y' positions for
      every node in the graph.}
    \item{getNodeHeight}{\code{signature(object = "Ragraph")}: Returns a
      vector with the heights of every node in the graph}
    \item{getNodeLW}{\code{signature(object = "Ragraph")}: Returns a
      vector with the left width of every node in the graph.}
    \item{getNodeRW}{\code{signature(object = "Ragraph")}: Returns a
      vector with the right width of every node in the graph.}
  }
}
\author{Jeff Gentry}

\seealso{\code{\link{agopen}}}

\examples{
set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, .2)
z <- agopen(g1,"foo")
z

## The various methods in action

## These methods are all used to obtain positional information about nodes
getNodeXY(z)
getNodeHeight(z)
getNodeLW(z)
getNodeRW(z)

## Retrieve information about the edges in the graph
edgeNames(z)
edgemode(z)

## These get information about the layout
laidout(z)
layoutType(z)
boundBox(z)

## Used to retrieve the entire list of edges or nodes
AgEdge(z)
AgNode(z)
}
\keyword{classes}
