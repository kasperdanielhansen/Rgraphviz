.First.lib <- function(lib, pkg, where) {
    if (missing(where)) {
        where <- match(paste("package:", pkg, sep=""), search())
        if(is.na(where)) {
            warning(paste("Not a package name: ",pkg))
            return()
        }
        where <- pos.to.env(where)
    }

    require(graph)
    library.dynam( "Rgraphviz", pkg, lib )
    .Call("Rgraphviz_init")
    .initRagraph(where)
    .initEdgePoints(where)
    .initControlPoints(where)
    .initXYPoint(where)
}
