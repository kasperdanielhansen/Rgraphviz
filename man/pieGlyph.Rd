\name{pieGlyph}
\alias{pieGlyph}
\title{A function to plot pie graphs as a glyph}
\description{
  This function allows the user to plot a pie graph at a specified x/y
  location in a plotting region.
}
\usage{
pieGlyph(x, xpos, ypos, labels = names(x), edges = 200, radius = 0.8, density = NULL, angle = 45, col = NULL, border = NULL, lty = NULL, main = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xpos}{The x location of the glyph}
  \item{ypos}{The Y location of the glyph}
  \item{x}{a vector of positive quantities.
    The values in \code{x} are displayed as the areas of pie slices.}
  \item{labels}{a vector of character strings giving names for the
    slices.  For empty or NA labels, no pointing line is drawn either.}
  \item{edges}{the circular outline of the pie is approximated by a
    polygon with this many edges.}
  \item{radius}{the pie is drawn centered in a square box whose sides
    range from \eqn{-1} to \eqn{1}.  If the character strings labeling
    the slices are long it may be necessary to use a smaller radius.}
  \item{density}{the density of shading lines, in lines per inch.
    The default value of \code{NULL} means that no shading lines
    are drawn. Non-positive values of \code{density} also inhibit the
    drawing of shading lines.}
  \item{angle}{the slope of shading lines, given as an angle in
    degrees (counter-clockwise).}
  \item{col}{a vector of colors to be used in filling or shading
    the slices. If missing a set of 6 pastel colours is used,
    unless \code{density} is specified when \code{par("fg")} is used.}
  \item{border, lty}{(possibly vectors) arguments passed to
    \code{\link{polygon}} which draws each slice.}
  \item{main}{an overall title for the plot.}
  \item{\dots}{graphical parameters can be given as arguments to
    \code{pie}.  They will affect the main title and labels only.}
}
\author{R. Gentleman, F. Sim}
\seealso{\code{\link{pie}}}
\examples{
   plot(1:10, col="white")
   pieGlyph(1:20, 5, 5)
}
\keyword{aplot}
\keyword{graphs}
