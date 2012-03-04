setGeneric("getX", function(object) standardGeneric("getX"))

setGeneric("getY", function(object) standardGeneric("getY"))

setGeneric("getPoints", function(object) standardGeneric("getPoints"))

setGeneric("labelText", function(object) standardGeneric("labelText"))

setGeneric("labelColor", function(object) standardGeneric("labelColor"))

setGeneric("labelLoc", function(object) standardGeneric("labelLoc"))

setGeneric("labelJust", function(object) standardGeneric("labelJust"))

setGeneric("labelWidth", function(object) standardGeneric("labelWidth"))

setGeneric("labelFontsize", function(object) standardGeneric("labelFontsize"))

setGeneric("botLeft", function(object) standardGeneric("botLeft"))

setGeneric("upRight", function(object) standardGeneric("upRight"))

setGeneric("shape", function(object) standardGeneric("shape"))

setGeneric("style", function(object) standardGeneric("style"))

setGeneric("color", function(object) standardGeneric("color"))

setGeneric("fillcolor", function(object) standardGeneric("fillcolor"))

setGeneric("getNodeCenter", function(object) standardGeneric("getNodeCenter"))

setGeneric("getNodeHeight", function(object) standardGeneric("getNodeHeight"))

setGeneric("getNodeRW", function(object) standardGeneric("getNodeRW"))

setGeneric("getNodeLW", function(object) standardGeneric("getNodeLW"))

setGeneric("name", function(object) standardGeneric("name"))

setGeneric("txtLabel", function(object) standardGeneric("txtLabel"))

setGeneric("border.color", function(object) standardGeneric("border.color"))

setGeneric("border.lwd", function(object) standardGeneric("border.lwd"))

setGeneric("getNodeXY", function(object) standardGeneric("getNodeXY"))

setGeneric("arrowsize", function(object) standardGeneric("arrowsize"))

setGeneric("arrowhead", function(object) standardGeneric("arrowhead"))

setGeneric("arrowtail", function(object) standardGeneric("arrowtail"))

setGeneric("splines", function(object) standardGeneric("splines"))

setGeneric("sp", function(object) standardGeneric("sp"))

setGeneric("ep", function(object) standardGeneric("ep"))

setGeneric("numSplines", function(object) standardGeneric("numSplines"))

setGeneric("getSpline", function(object, pos) standardGeneric("getSpline"))

setGeneric("cPoints", function(object) standardGeneric("cPoints"))

setGeneric("pointList", function(object) standardGeneric("pointList"))

setGeneric("bezierPoints", function(object) standardGeneric("bezierPoints"))

setGeneric("bLines", function(x, ...) standardGeneric("bLines"))

setGeneric("agraph", function(object) standardGeneric("agraph"))

setGeneric("laidout", function(object) standardGeneric("laidout"))

setGeneric("layoutType", function(object) standardGeneric("layoutType"))

setGeneric("boundBox", function(object) standardGeneric("boundBox"))

setGeneric("AgEdge", function(object) standardGeneric("AgEdge"))

setGeneric("AgEdge<-", function(object, value) standardGeneric("AgEdge<-"))

setGeneric("AgNode", function(object) standardGeneric("AgNode"))

setGeneric("AgNode<-", function(object, value) standardGeneric("AgNode<-"))

setGeneric("from", function(object) standardGeneric("from"))

setGeneric("to", function(object) standardGeneric("to"))

setGeneric("toDot", function(graph, filename, ...) standardGeneric("toDot"))

###################################################################
# graph attributes
###################################################################

setGeneric("graphDataDefaults", function(self)
           standardGeneric("graphDataDefaults"))

setGeneric("graphDataDefaults<-", function(self, attr, value)
           standardGeneric("graphDataDefaults<-"))

setGeneric("graphData", function(self, attr)
           standardGeneric("graphData"))

setGeneric("graphData<-", function(self, attr, value)
           standardGeneric("graphData<-"))

###################################################################
# cluster attributes
###################################################################

setGeneric("clusterData", function(self, cluster, attr)
           standardGeneric("clusterData"))

setGeneric("clusterData<-", function(self, cluster, attr, value)
           standardGeneric("clusterData<-"))

