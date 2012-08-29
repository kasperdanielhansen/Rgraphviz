toFile <- function(graph, 
                   layoutType = graphvizCapabilities()$layoutTypes,
                   filename, 
                   fileType = graphvizCapabilities()$deviceTypes) {
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   
   layoutType <- match.arg(layoutType)
   fileType <- match.arg(fileType)
   if(!is.null(graphvizCapabilities()) &&
      ! fileType %in% graphvizCapabilities()$deviceTypes) {
       warning("Graphviz does not support 'fileType'")
       return(NULL)
   }

   .Call("Rgraphviz_toFile", graph, 
		as.character(layoutType), 
		as.character(filename), 
		as.character(fileType), 
		PACKAGE="Rgraphviz2")
}

setMethod("toDot", "graph", function(graph, filename, ...) {
    z <- agopen(graph, name = "foo", ...)
    agwrite(z, filename)
})

agwrite <- function(graph, filename)
{
    g <- .Call("Rgraphviz_agwrite", graph, as.character(filename),
               PACKAGE="Rgraphviz2")
    g
}


