.First.lib <- function(lib, pkg, where) {
    library.dynam( "Rgraphviz", pkg, lib )
    .Call("Rgraphviz_init")

    require("graph") || stop ("Rgraphviz requires package graph.")
    .initgraphMethods()
    .initRgraphvizLineMethods()
    .initRgraphvizShowMethods()
    .initRgraphvizPlotMethods()
    if(.Platform$OS.type == "windows" && require("Biobase") && interactive()
        && .Platform$GUI ==  "Rgui"){
        addPDF2Vig("widgetTools")
    }
}

