\name{clusterData-methods}
\docType{methods}
\alias{clusterData-methods}
\alias{clusterData<--methods}
\alias{clusterData}
\alias{clusterData<-}

\title{Get and set attributes for a cluster of an Ragraph object}

\usage{
clusterData(self, cluster, attr)
clusterData(self, cluster, attr) <- value
}
\arguments{
  \item{self}{A \code{Ragraph-class} instance}
  \item{cluster}{cluster number}
  \item{attr}{A \code{character} vector of length one specifying the
  name of a cluster attribute}
  \item{value}{A \code{character} vector to store as the attribute value}
}

\description{
  Attributes of a graph can be accessed using \code{clusterData}.  
  There's no default attributes for clusters.
  The attributes must be defined using \code{\link{graphDataDefaults}}.  

}

\author{Li Long <li.long@isb-sib.ch>}

\examples{

library(graph)
library(Rgraphviz)

g1_gz <- gzfile(system.file("GXL/graphExample-01.gxl.gz",package="graph"), open="rb")
g11_gz <- gzfile(system.file("GXL/graphExample-11.gxl.gz",package="graph"), open="rb")
g1 <- fromGXL(g1_gz)
g11 <- fromGXL(g11_gz)
g1_11 <- join(g1, g11)
sgl <- vector(mode="list", length=2)
sgl[[1]] <- list(graph=g1, cluster=TRUE)
sgl[[2]] <- list(graph=g11, cluster=TRUE)
ng <- agopenSimple(g1_11, "tmpsg", subGList=sgl)
clusterData(ng, 1, c("bgcolor")) <- c("blue")
clusterData(ng, 2, c("bgcolor")) <- c("red")
toFile(ng, layoutType="dot", filename="g1_11_dot.ps", fileType="ps")

}
\keyword{methods}

