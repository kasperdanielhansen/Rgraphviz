.onLoad <- function(libname, pkgname) {
    tryCatch({
        library.dynam('Rgraphviz', pkgname, libname)
    }, error=function(err) {
        msg <- sprintf("
            Rgraphviz is unable to be loaded.  This typically
            is a symptom of an installation problem.  From 2.x.x
            onwards, Graphviz ought to come bundled with Rgraphviz.

            If you are trying to use an external Graphviz, see
            additional instructions in the 'README' file of the
            Rgraphviz 'source' distribution, available at

            http://bioconductor.org/packages/release/bioc/html/Rgraphviz.html

            Ask further questions on the Bioconductor mailing list

            http://bioconductor.org/docs/mailList.html

            ", as.character(Rgraphviz:::.graphviz_build_version))
        stop(conditionMessage(err), "\n\n",
             paste(strwrap(msg, indent=2, exdent=2), collapse="\n"))
    })
    .Call("Rgraphviz_init", PACKAGE = "Rgraphviz")
}
