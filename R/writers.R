toFile <- function(graph, layoutType = "dot", filename, fileType = "dot") {
   if ( !is(graph,"Ragraph") ) stop("Given graph is not of class Ragraph")
   
   layoutType <- .checkLayoutType(layoutType)
   fileType <- .checkFileType(fileType)
   filename <- path.expand(filename)
   if(!is.null(graphvizCapabilities()) &&
      ! fileType %in% graphvizCapabilities()$deviceTypes) {
       warning("Graphviz does not support 'fileType'")
       return(NULL)
   }

   .Call("Rgraphviz_toFile", graph, 
		as.character(layoutType), 
		as.character(filename), 
		as.character(fileType), 
		PACKAGE="Rgraphviz")
}

setMethod("toDot", "graph", function(graph, filename, ...) {
    z <- agopen(graph, name = "foo", ...)
    agwrite(z, filename)
})

agwrite <- function(graph, filename)
{
    filename <- path.expand(filename)
    if(file.exists(filename))
        unlink(filename)
    g <- .Call("Rgraphviz_agwrite", graph, as.character(filename),
               PACKAGE="Rgraphviz")
    g
}


