test_layout <- function() {
    layoutTypes <- graphvizCapabilities()$layoutTypes
    names(layoutTypes) <- layoutTypes
    mat <- rbind(c(0, 0, 1, 1),
                 c(0, 0, 1, 1),
                 c(1, 1, 0, 1),
                 c(1, 1, 1, 0))
    rownames(mat) <- colnames(mat) <- letters[1:4]
    g1 <- new("graphAM", adjMat = mat)
    mat[3:4,1] <- mat[1, 3:4] <- 0
    g2 <- new("graphAM", adjMat = mat) # g2 has an isolated node
    tmpfile <- tempfile()
    checkTrue(file.create(tmpfile))
    on.exit(unlink(tmpfile))
    pdf(file = tmpfile)
    cat("plotting different layout types\n")
    for(lType in layoutTypes) {
        cat("  ", lType, " ")
        lay1 <- layoutGraph(g1, layoutType = lType)
        cat(".")
        ren1 <- renderGraph(lay1)
        cat(".")
        lay2 <- layoutGraph(g2, layoutType = lType)
        cat(".")
        ren2 <- renderGraph(lay2)
        cat(". done\n")
    }
}
