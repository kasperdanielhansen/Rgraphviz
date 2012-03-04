makeNodeAttrs <- function(g, label=nodes(g), shape="ellipse", fillcolor="#e0e0e0", border.lwd=1, border.color='black', ...) {
  rv = list(label=label, shape=shape, fillcolor=fillcolor, border.lwd=border.lwd, border.color=border.color, ...)
  for(i in seq(along=rv)) {
    if(length(rv[[i]])==1) {
      rv[[i]] = rep(rv[[i]], numNodes(g))
    } else {
      if(length(rv[[i]])!=numNodes(g))
        stop("Attribute vector must have as many elements as 'g' has nodes.")
    }
    names(rv[[i]]) <- nodes(g)
  }
  return(rv)
}
