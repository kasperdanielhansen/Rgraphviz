.First.lib <- function(lib, pkg, where) {
    library.dynam( "Rgraphviz", pkg, lib )
    .Call("Rgraphviz_init")

    require(graph) || stop ("Rgraphviz requires package graph.")
    .initgraphMethods()
    .initRgraphvizLineMethods()
    .initRgraphvizShowMethods()
    .initRgraphvizPlotMethods()
}

