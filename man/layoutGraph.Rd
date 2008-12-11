\name{layoutGraph}
\alias{layoutGraph}
\title{ A function to compute layouts of  graph objects }

\description{
  
  This is a wrapper to layout graph objects using arbitrary layout
  engines. The default engine (and so far the only implemented engine)
  is ATT's Graphviz.
  
}

\usage{

layoutGraph(x, layoutFun = layoutGraphviz, ...)

}

\arguments{
  
  \item{x}{ A graph object }
  
  \item{layoutFun}{ A function that performs the graph layout and
    returns a graph object with all necessary rendering information }
  
  \item{\dots}{ Further arguments that are passed to \code{layoutFun} }
  
}

\details{
  
  Layout of a graph and its rendering are two separate
  processes. \code{layoutGraph} provides an API to use an arbitrary
  algorithm for the layout. This is archived by abstraction of the
  layout process into a separate function (\code{layoutFun}) with
  well-defined inputs and outputs. The only requirements on the
  \code{layoutFun} are to accept a graph object as input and to return a
  valid graph object with all the necessary rendering information stored
  in its \code{renderInfo} slot. This information comprises

  \strong{for nodes:}

  \describe{
  
    \item{nodeX, nodeY}{the locations of the nodes, in the coordinate
      system defined by \code{bbox} (see below). }
    
    \item{lWidth, rWidth}{the width components of the nodes,
      \code{lWidth+rWidth=total width}. }
    
    \item{height}{the heights of the nodes. }

    \item{labelX, labelY}{node label locations. }

    \item{labelJust}{the justification of the node labels. }
    
    \item{label}{node label text. }
    
    \item{shape}{the node shape. Valid values are \code{box},
      \code{rectangle}, \code{ellipse}, \code{plaintext}, \code{circle}
      and \code{triangle}. }
  
  }
  
  \strong{for edges:}

  \describe{
    
    \item{splines}{representation of the edge splines as a list of
      \code{\link{BezierCurve}} objects. }
    
    \item{labelX, labelY}{edge label locations. }
    
    \item{label}{edge label text. }
    
    \item{arrowhead, arrowtail}{some of Graphviz's arrow shapes are
      supported. Currently they are: \code{open}, \code{normal},
      \code{dot}, \code{odot}, \code{box}, \code{obox}, \code{tee},
      \code{diamond}, \code{odiamond} and \code{none}. In addition, a
      user-defined function can be passed which needs to be able to deal
      with 4 arguments: A list of xy coordinates for the center of the
      arrowhead, and the graphical parameters \code{col}, \code{lwd} and
      \code{lty}.}
  
    \item{direction}{The edge direction. The special value \code{both} is
      used when reciprocrated edges are to be collapsed. }

  }

  To indicate that this information has been added to the graph, the
  graph plotting function should also set the laidout flag in the
  \code{graphData} slot to \code{TRUE} and add the bounding box
  information (i.e., the coordinate system in which the graph is laid
  out) in the format of a two-by-two matrix as item \code{bbox} in the
  \code{graphData} slot.

  AT&T's \code{Graphviz} is the default layout algorithm to use when
  \code{layoutGraph} is called without a specific \code{layoutFun}
  function. See \code{\link{agopen}} for details about how to tweak
  \code{Graphviz} and the valid arguments that can be passed on through
  \dots. The most common ones to set in this context might be
  \code{layoutType}, which controls the type of layout to compute and
  the \code{nodeAttrs} and \code{edgeAttrs} arguments, which control the
  fine-tuning of nodes and edges.
  
}

\note{

  Please note that the layout needs to be recomputed whenever attributes
  are changed which are bound to affect the position of nodes or
  edges. This is for instance the case for the \code{arrowhead} and
  \code{arrowtail} parameters.

}


\value{
  
  An object inheriting from class \code{\link[graph]{graph}}
  
}

\author{ Florian Hahne, Deepayan Sarkar }


\seealso{ \code{\link[graph]{renderGraph}},
  \code{\link[graph]{graph.par}},
  \code{\link[graph]{nodeRenderInfo}},
  \code{\link[graph]{edgeRenderInfo}},
  \code{\link{agopen}},
}

\examples{
library(graph)
set.seed(123)
V <- letters[1:5]
M <- 1:2
g1 <- randomGraph(V, M, 0.5)
edgemode(g1) <- "directed"
x <- layoutGraph(g1)
renderGraph(x)

## one of Graphviz's additional layout algorithms
x <- layoutGraph(g1, layoutType="neato")
renderGraph(x)

## some tweaks to Graphviz's node and edge attributes,
## including a user-defined arrowhead and node shape functions.
myArrows <- function(x, ...)
{
for(i in 1:4)
points(x,cex=i)
}

myNode <- function(x, col, fill, ...)
symbols(x=mean(x[,1]), y=mean(x[,2]), thermometers=cbind(.5, 1,
runif(1)), inches=0.5,
fg=col, bg=fill, add=TRUE)

eAtt <- list(arrowhead=c("a~b"=myArrows, "b~d"="odiamond", "d~e"="tee"))
nAtt <- list(shape=c(d="box", c="ellipse", a=myNode))
edgemode(g1) <- "directed"
x <- layoutGraph(g1, edgeAttrs=eAtt, nodeAttrs=nAtt, layoutType="neato")
renderGraph(x)

}

\keyword{ graphs }

