.First.lib <- function(lib, pkg, where) {
    library.dynam( "Rgraph", pkg, lib )
    .Call("Rgraph_init")
}
