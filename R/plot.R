# TODO:
#     not really needed outside buildEdgeList/buildNodeList, 
#     probably should remove them from "export"
#     related files: 
#          NAMESPACE, plot.R, pEdge.Rd pNode.Rd, buildNodeList.Rd, example 
##Copyright R. Gentleman 2003, all rights reserved

setMethod("name", "pNode", function(object) object@name)

setMethod("from", "pEdge", function(object) object@from)

setMethod("to", "pEdge", function(object) object@to)


