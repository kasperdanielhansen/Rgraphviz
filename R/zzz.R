.First.lib <- function(lib, pkg, where) {
    if (missing(where)) {
        where <- match(paste("package:", pkg, sep=""), search())
        if(is.na(where)) {
            warning(paste("Not a package name: ",pkg))
            return()
        }
        where <- pos.to.env(where)
    }

    library.dynam( "Rgraphviz", pkg, lib )
    .Call("Rgraphviz_init")

    require(graph) || stop ("Rgraphviz requires package graph.")

    .initRgraphvizMethods(where)
    .initGraphPlotClasses(where)
}

