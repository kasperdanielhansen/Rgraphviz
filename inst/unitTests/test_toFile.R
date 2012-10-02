test_toFile <- function() {
    deviceTypes <- graphvizCapabilities()$deviceTypes
    names(deviceTypes) <- deviceTypes
    mat <- rbind(c(0, 0, 1, 1),
                 c(0, 0, 1, 1),
                 c(1, 1, 0, 1),
                 c(1, 1, 1, 0))
    rownames(mat) <- colnames(mat) <- letters[1:4]
    g1 <- new("graphAM", adjMat = mat)
    ag1 <- agopen(g1, name = "foo")
    tmpfile <- tempfile()
    checkTrue(file.create(tmpfile))
    on.exit(unlink(tmpfile))
    cat("using toFile with various device types\n")
    for(dType in deviceTypes) {
        cat("  ", dType)
        toFile(ag1, filename = tmpfile, layoutType = "dot", fileType = dType)
        cat(" . done\n")
    }
}

test_toDotRead <- function() {
    mat <- rbind(c(0, 0, 1, 1),
                 c(0, 0, 1, 1),
                 c(1, 1, 0, 1),
                 c(1, 1, 1, 0))
    rownames(mat) <- colnames(mat) <- letters[1:4]
    g1 <- new("graphAM", adjMat = mat)
    ag1 <- agopen(g1, name = "foo")
    tmpfile <- tempfile()
    checkTrue(file.create(tmpfile))
    on.exit(unlink(tmpfile))
    toDot(g1, filename = tmpfile)
    ag1 <- agread(filename = tmpfile)
}

test_toFileRead <- function() {
    mat <- rbind(c(0, 0, 1, 1),
                 c(0, 0, 1, 1),
                 c(1, 1, 0, 1),
                 c(1, 1, 1, 0))
    rownames(mat) <- colnames(mat) <- letters[1:4]
    g1 <- new("graphAM", adjMat = mat)
    ag1 <- agopen(g1, name = "foo")
    tmpfile <- tempfile()
    checkTrue(file.create(tmpfile))
    on.exit(unlink(tmpfile))
    toFile(ag1, filename = tmpfile, layoutType = "dot", fileType = "dot")
    ag1 <- agread(filename = tmpfile)
}

