.First.lib <- function(lib, pkg, where) {
    require(graph)
    library.dynam( "Rgraphviz", pkg, lib )
    .Call("Rgraphviz_init")
}
