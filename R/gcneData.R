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

setMethod("graphDataDefaults",
          signature(self="Ragraph"),
          function(self) {
             getDefAttrsGraph(self)
          })

setReplaceMethod("graphDataDefaults",
          signature(self="Ragraph", attr="vector", value="vector"),
          function(self, attr, value) {
             setDefAttrsGraph(self, attr, value);
             self
          })

setMethod("graphData",
          signature(self="Ragraph", attr="vector"),
          function(self, attr) {
             ans <- getAttrsGraph(self, attr)
             ans[which(ans==FALSE)] = NA
             ans
          })

setReplaceMethod("graphData",
          signature(self="Ragraph", attr="vector", value="vector"),
          function(self, attr, value) {
             ans <- getAttrsGraph(self, attr)
             if ( any(ans==FALSE) )
             {
                stop("No default for some attribute(s), set them up first")
             }
             else
             {
                setAttrsGraph(self, attr, value)
                self
             }
          })

###################################################################
# cluster attributes
###################################################################

setGeneric("clusterDataDefaults", function(self, cluster)
           standardGeneric("clusterDataDefaults"))

setGeneric("clusterDataDefaults<-", function(self, cluster, attr, value)
           standardGeneric("clusterDataDefaults<-"))

setGeneric("clusterData", function(self, cluster, attr)
           standardGeneric("clusterData"))

setGeneric("clusterData<-", function(self, cluster, attr, value)
           standardGeneric("clusterData<-"))

###################################################################

setMethod("clusterDataDefaults",
          signature(self="Ragraph", cluster="numeric"),
          function(self, cluster) {
             getDefAttrsCluster(self, cluster)
          })

setReplaceMethod("clusterDataDefaults",
          signature(self="Ragraph", cluster="numeric", attr="vector", value="vector"),
          function(self, cluster, attr, value) {
             setDefAttrsCluster(self, cluster, attr, value);
             self
          })

setMethod("clusterData",
          signature(self="Ragraph", cluster="numeric", attr="vector"),
          function(self, cluster, attr) {
             ans <- getAttrsCluster(self, cluster, attr)
             ans[which(ans==FALSE)] = NA
             ans
          })

setReplaceMethod("clusterData",
          signature(self="Ragraph", cluster="numeric", attr="vector", value="vector"),
          function(self, cluster, attr, value) {
             ans <- getAttrsCluster(self, cluster, attr)
             if ( any(ans==FALSE) )
             {
                stop("No default for some attribute(s), set them up first")
             }
             else
             {
                setAttrsCluster(self, cluster, attr, value)
                self
             }
          })

###################################################################
# node attributes
###################################################################
#
#setGeneric("nodeDataDefaults", function(self)
#           standardGeneric("nodeDataDefaults"))
#
#setGeneric("nodeDataDefaults<-", function(self, attr, value)
#           standardGeneric("nodeDataDefaults<-"))
#
#setGeneric("nodeData", function(self, n, attr)
#           standardGeneric("nodeData"))
#
#setGeneric("nodeData<-", function(self, n, attr, value)
#           standardGeneric("nodeData<-"))
#
###################################################################

setMethod("nodeDataDefaults", 
          signature(self="Ragraph", attr="missing"),
          function(self) {
	     getDefAttrsNode(self)
          })

setReplaceMethod("nodeDataDefaults", 
          signature(self="Ragraph", attr="vector", value="vector"),
          function(self, attr, value) {
	     setDefAttrsNode(self, attr, value);
             self
          })

setMethod("nodeData",
          signature(self="Ragraph", n="vector", attr="vector"),
          function(self, n, attr) {
             ans <- getAttrsNode(self, n, attr)
             ans[which(ans==FALSE)] = NA
             ans
          })

setReplaceMethod("nodeData",
          signature(self="Ragraph", n="vector", attr="vector", value="vector"),
          function(self, n, attr, value) {
             ans <- getAttrsNode(self, n, attr)
             if ( any(ans==FALSE) )
             {
                stop("No default for some attribute(s), set them up first")
             }
             else
             {
                setAttrsNode(self, n, attr, value)
                self
             }
          })

###################################################################
# edge attributes
###################################################################
#
#setGeneric("edgeDataDefaults", function(self)            
#	   standardGeneric("edgeDataDefaults"))
#
#setGeneric("edgeDataDefaults<-", function(self, attr, value)
#           standardGeneric("edgeDataDefaults<-"))
#
#setGeneric("edgeData", function(self, from, to, attr)
#           standardGeneric("edgeData"))
#
#setGeneric("edgeData<-", function(self, from, to, attr, value)
#           standardGeneric("edgeData<-"))
#
###################################################################

setMethod("edgeDataDefaults", 
          signature(self="Ragraph", attr="missing"),
          function(self) {
             getDefAttrsEdge(self)
	  })

setReplaceMethod("edgeDataDefaults", 
          signature(self="Ragraph", attr="vector", value="vector"),
          function(self, attr, value) {
             setDefAttrsEdge(self, attr, value)
             self
          })

setMethod("edgeData", 
          signature(self="Ragraph", from="vector", to="vector", attr="vector"),
          function(self, from, to, attr) {
             ans <- getAttrsEdge(self, from, to, attr)
             ans[which(ans==FALSE)] = NA
             ans
          })

setReplaceMethod("edgeData",
          signature(self="Ragraph", from="vector", to="vector",
                    attr="vector", value="vector"),
          function(self, from, to, attr, value) {
             ans <- getAttrsEdge(self, from, to, attr)
             if ( any(ans==FALSE) )
             {
                stop("No default for some attribute(s), set them up first")
             }
             else
             {
                setAttrsEdge(self, from, to, attr, value)
                self
             }
          })


