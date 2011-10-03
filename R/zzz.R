.onLoad <- function(libname, pkgname) {
    tryCatch({
        library.dynam('Rgraphviz', pkgname, libname)
    }, error=function(err) {
        msg <- sprintf("
            Check that (1) graphviz is installed on your system; (2)
            the installed version of graphviz matches '%s'; this is
            the version used to build this Rgraphviz package; (3)
            graphviz is accessible to R, e.g., the path to the
            graphviz 'bin' directory is in the system 'PATH' variable.
            See additional instructions in the 'README' file of the
            Rgraphviz 'source' distribution, available at

            http://bioconductor.org/packages/release/bioc/html/Rgraphviz.html

            Ask further questions on the Bioconductor mailing list

            http://bioconductor.org/docs/mailList.html

            ", as.character(Rgraphviz:::.graphviz_build_version))
        stop(conditionMessage(err), "\n\n",
             paste(strwrap(msg, indent=2, exdent=2), collapse="\n"))
    })
    .Call("Rgraphviz_init", PACKAGE = "Rgraphviz")
    versions <- graphvizVersion()
    if(versions$installed_version != versions$build_version) {
        warning("Rgraphviz built with Graphviz version ",
                versions$build_version,
                ".\nFound installed Graphviz version ",
                versions$installed_version, ". This _may_ cause problems.")
    }
}
