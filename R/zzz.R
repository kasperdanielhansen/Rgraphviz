.First.lib <- function(lib, pkg, where) {
    if (missing(where)) {
        where <- match(paste("package:", pkgname, sep=""), search())
        if(is.na(where)) {
            warning(paste("Not a package name: ",pkgname))
            return()
        }
        where <- pos.to.env(where)
    }

    require(graph)
    library.dynam( "Rgraphviz", pkg, lib )
    .Call("Rgraphviz_init")
    .initRagraph(where)
}
