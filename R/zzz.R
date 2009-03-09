.onLoad <- function(lib, pkg, where) {
    library.dynam( "Rgraphviz", pkg, lib)
    .Call("Rgraphviz_init", PACKAGE = "Rgraphviz")
    versions <- graphvizVersion()
    if(versions$installed_version != versions$build_version) {
        warning("Rgraphviz built with Graphviz version ",
                versions$build_version,
                ".\nFound installed Graphviz version ",
                versions$installed_version, ". This _may_ cause problems.")
    }
    require("methods")
}
