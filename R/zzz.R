.First.lib <- function(lib, pkg, where) {
    library.dynam( "Rgraphviz", pkg, lib )
    .Call("Rgraphviz_init")
}
