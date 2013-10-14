\name{renderGraph}
\docType{methods}
\alias{renderGraph}
\alias{renderGraph,graph-method}
\title{Render a laid out graph object}

\description{
  
  This method uses the \code{renderInfo} slot of a graph object to
  render it on a plotting device. The graph must have been laid out
  using the \code{\link{layoutGraph}} function before. 
  
}

\usage{

\S4method{renderGraph}{graph}(x, \dots, drawNodes="renderNodes", drawEdges=renderEdges, graph.pars=list())

}

\arguments{
  
  \item{x}{An object derived from class \code{graph}}
  
  \item{drawNodes}{A function that is used for the node rendering. The
    details of its API are still undecided, so far the input to the
    function is the (laid out) graph object. Defaults to
    \code{renderNodes}, which is not exported in the name space (type
    \code{Rgraphviz:::renderNodes} to see the function definition). This
    default function knows how to draw node shapes of type \code{box},
    \code{rectangle}, \code{ellipse}, \code{plaintext}, \code{circle}
    and \code{triangle}. In addition, an arbitrary user-defined function
    can be passed on to draw the node. This function needs to be able to
    deal with the following arguments: a two-by-two matrix of the
    bounding box for the respective node, and \code{labelX},
    \code{labelY}, \code{fill}, \code{col}, \code{lwd}, \code{lty},
    \code{textCol}, \code{style}, \code{label} and \code{fontsize},
    which are all defined by the layout algorithm or are graphical
    \code{nodeRenderInfo} parameters. }
      
  \item{drawEdges}{A function that is used for the edge
    rendering. Defaults to \code{renderEdges}. Currently, this function
    can draw different types of arrowheads: \code{open}, \code{normal},
    \code{dot}, \code{odot}, \code{box}, \code{obox}, \code{tee},
    \code{diamond}, \code{odiamond} and \code{none}. In addition, a
    user-defined function can be passed as \code{arrowhead} or
    \code{arrowtail} parameters which needs to be able to deal with 4
    arguments: A list of xy coordinates for the center of the arrowhead,
    and the graphical parameters \code{col}, \code{lwd} and
    \code{lty}. }
  
  \item{graph.pars}{A list of rendering parameters to use as
    defaults. Parameters that have been explicitely set using
    \code{\link[graph:renderInfo-class]{nodeRenderInfo}},
    \code{\link[graph:renderInfo-class]{edgeRenderInfo}} or
    \code{\link[graph:renderInfo-class]{graphRenderInfo}} take
    precendence. If not explicitely supplied, the value of a call to
    \code{\link[graph:settings]{graph.par}} is used. This design allows
    to set session-wide defaults. }
  
  \item{\dots}{further arguments}
  
}

\details{
  
  This method can render graph objects that have previously been laid
  out using the function \code{\link{layoutGraph}}. The details for user
  defined node drawing remain to be decided.
  
}

\value{
  
  An object derived from class \code{graph} with information about the
  coordinates of the nodes in the coordinate system of the plotting
  device added to the \code{renderInfo} slot.
  
}

\author{Florian Hahne}

\seealso{

  \code{\link{layoutGraph}},
   \code{link[graph:renderInfo-class]{nodeRenderInfo}}, 
   \code{link[graph:renderInfo-class]{edgeRenderInfo}},
   \code{link[graph:renderInfo-class]{graphRenderInfo}},
  
}

\examples{

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

\keyword{methods}
