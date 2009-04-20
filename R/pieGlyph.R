pieGlyph <- function (x, xpos, ypos, labels = names(x), edges = 200,
    radius = 0.8, density = NULL, angle = 45, col = NULL, border =
    NULL, lty = NULL, main = NULL, ...)
{
    if (!is.numeric(x) || any(is.na(x) | x <= 0))
        stop("pie: `x' values must be positive.")
    if (is.null(labels))
        labels <- as.character(1:length(x))
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    if (is.null(col))
        col <- if (is.null(density))
            c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk", "white")
        else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    for (i in 1:nx) {
        n <- max(2, floor(edges * dx[i]))
        t2p <- 2 * pi * seq(x[i], x[i + 1], length = n)
        xc <- c(cos(t2p), 0) * radius+xpos
        yc <- c(sin(t2p), 0) * radius+ypos
        polygon(xc, yc, density = density[i], angle = angle[i],
            border = border[i], col = col[i], lty = lty[i])
        ## plot labels (this is a patch by Fraser Sim)
        t2p <- 2 * pi * mean(x[i + 0:1])
        xc <- cos(t2p) * radius * c(1,1.1,1.2) + xpos
        yc <- sin(t2p) * radius * c(1,1.1,1.2) + ypos
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) { 
            lines(xc[1:2], yc[1:2])
            text(xc[3], yc[3], labels[i], xpd = TRUE, 
                 adj = ifelse(xc < xpos, 1, ifelse(xc == xpos, 0.5,
                                                   0)), ...)
        }
    }
    invisible(NULL)
}
