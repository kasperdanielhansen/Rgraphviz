.onLoad <- function(lib, pkg, where) {
    library.dynam( "Rgraphviz", pkg, lib )
    .Call("Rgraphviz_init", PACKAGE="Rgraphviz")
	if (.Platform$OS.type == "windows") {
	    .graphviz_installed_version <-
	      as.list(numeric_version(.Call("Rgraphviz_graphvizVersion", PACKAGE="Rgraphviz")))
	    .graphviz_installed_version[[1]] <- .graphviz_installed_version[[1]][1:2]
	    oldClass(.graphviz_installed_version) <- "numeric_version"
	    if (.graphviz_installed_version != .graphviz_build_version)
	        warning("Rgraphviz built with Graphviz version ",
	                .graphviz_build_version,
	                ".\nFound Graphviz version ",
	                .graphviz_installed_version,
	                ".")
    }
    require("methods")
}
