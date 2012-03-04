\name{AgNode-class}
\docType{class}
\alias{AgNode-class}
\alias{AgNode}
\alias{AgNode<-}
\alias{color}
\alias{fillcolor}
\alias{name}
\alias{shape}
\alias{getNodeRW}
\alias{getNodeLW}
\alias{getNodeCenter}
\alias{getRadiusDiv}
\alias{getNodeHeight}
\alias{style}
\alias{style,AgNode-method}
\alias{color,AgNode-method}
\alias{fillcolor,AgNode-method}
\alias{getNodeXY,AgNode-method}
\alias{getNodeCenter,AgNode-method}
\alias{getNodeHeight,AgNode-method}
\alias{getNodeLW,AgNode-method}
\alias{getNodeRW,AgNode-method}
\alias{name,AgNode-method}
\alias{shape,AgNode-method}
\alias{txtLabel,AgNode-method}
\alias{drawAgNode}
\alias{drawCircleNode}
\title{Class "AgNode": A class to describe a node for a Ragraph object}
\description{This class is used to represent nodes for the
  \code{Ragraph} class.  One can retrieve various pieces of information
  as well as draw them.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AgNode", ...)}.
}
\section{Slots}{
  \describe{
    
    \item{\code{center}:}{Object of class \code{"xyPoint"}: The center
      point of the node}
    
    \item{\code{name}:}{Object of class \code{"character"}: The name of
      the node, used to reference it}
    
    \item{\code{txtLabel}:}{Object of class \code{"AgTextLabel"}: Label
      for this edge}
    
    \item{\code{height}:}{Object of class \code{"integer"}: Height of
      the node in points}
    
    \item{\code{rWidth}:}{Object of class \code{"integer"}: The right
      half of the node in points.}
    
    \item{\code{lWidth}:}{Object of class \code{"integer"}: The left
      half of the node in points.}
    
    \item{\code{color}:}{Object of class \code{"character"}: The drawing
      color of the node.}
    
    \item{\code{fillcolor}:}{Object of class \code{"character"}: The
      color to fill in the node with.}
    
    \item{\code{shape}:}{Object of class \code{"character"}: The shape
      of the node.}
    
    \item{\code{style}:}{Object of class \code{"character"}: The style
      of the node.}

    \item{\code{border.color}:}{Object of class \code{"character"}: The
      color of the border of the node.}

    \item{\code{border.lwd}:}{Object of class \code{"character"}: The
      line width of the border of the node.}

  }
}
\section{Methods}{
  \describe{
    \item{color}{\code{signature(object = "AgNode")}: Retrieves the
      drawing color for the node.}
    \item{fillcolor}{\code{signature(object = "AgNode")}: Retrieves the
      color to fill in the node image with.}
    \item{getNodeCenter}{\code{signature(object = "AgNode")}: Returns
      the center point of the node.}
    \item{getNodeXY}{\code{signature(object = "AgNode")}: Returns the
      center as a two element list, with the first element containing
      the 'x' value and the second element containing the 'y' value.}
    \item{getNodeHeight}{\code{signature(object = "AgNode")}: Returns
      the height of the node. }
    \item{getNodeLW}{\code{signature(object = "AgNode")}: Returns the
      left width of the node.}
    \item{getNodeRW}{\code{signature(object = "AgNode")}: Returns the
      right width of the node.}
    \item{name}{\code{signature(object = "AgNode")}: Retrieves the name
      of the node.}
    \item{shape}{\code{signature(object = "AgNode")}: Returns the shape
      of the node.}
    \item{style}{\code{signature(object = "AgNode")}: Returns the style
      of the node.}
    \item{txtLabel}{\code{signature(object = "AgNode")}: Retrieves the
      node label.}
    \item{border.color}{\code{signature(object = "AgNode")}: Retrieves the
      color of the border of the node.}
    \item{border.lwd}{\code{signature(object = "AgNode")}: Retrieves the
      line width of the border of the node.}
  }
}
\author{Jeff Gentry}
\seealso{\code{\link{Ragraph}}}
\examples{
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, .2)
z <- agopen(g1,name="foo")
x <- AgNode(z) ## list of AgNode objects
vv <- x[[1]]

## The methods in use
color(vv)
fillcolor(vv)
getNodeCenter(vv)
getNodeXY(vv)
getNodeHeight(vv)
getNodeLW(vv)
getNodeRW(vv)
name(vv)
shape(vv)
style(vv)
txtLabel(vv)
}
\keyword{classes}
