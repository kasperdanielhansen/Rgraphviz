.First.lib <- function(lib, pkg, where) {
    library.dynam( "Rgraphviz", pkg, lib )
    .Call("Rgraphviz_init", PACKAGE="Rgraphviz")

    require("graph") || stop ("Rgraphviz requires package graph.")
    .initRgraphvizLineMethods()
    .initRgraphvizShowMethods()
    .initRgraphvizPlotMethods()
}

