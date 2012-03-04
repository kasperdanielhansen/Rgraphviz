replaceAtt <- function(attributes, what, which, with)
{
    if (is.null(attributes[[what]][[which]]))
        attributes[[what]][[which]] <- with
    attributes
}

getDefaultAttrs <- function(curAttrs=list(),
                            layoutType=c("dot","neato","twopi","circo","fdp")) {
    layoutType <- match.arg(layoutType)
    if (length(curAttrs) == 0)
        curAttrs <- list(graph=list(), cluster=list(),
                         node=list(), edge=list())
    else {
        if (is.null(curAttrs$graph)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "graph"
        }
        if (is.null(curAttrs$cluster)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "cluster"
        }
        if (is.null(curAttrs$node)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "node"
        }
        if (is.null(curAttrs$edge)) {
            curAttrs[[length(curAttrs)+1]] <- list()
            names(curAttrs)[length(names(curAttrs))] <- "edge"
        }
    }

    bg <- "transparent"
    fg <- "black"
    col <- "black"

    ## Define the graph attributes
    curAttrs <- replaceAtt(curAttrs, "graph", "bgcolor", bg)
    curAttrs <- replaceAtt(curAttrs, "graph", "fontcolor", fg)
    curAttrs <- replaceAtt(curAttrs, "graph", "ratio", "fill")
    curAttrs <- replaceAtt(curAttrs, "graph", "overlap", "")
    curAttrs <- replaceAtt(curAttrs, "graph", "splines", "TRUE")
    curAttrs <- replaceAtt(curAttrs, "graph", "rank", "same")
   
    

    ## Use the 'fin' value for the Graphviz size, if there's no
    ## plot device open right now, then use a sensible default
    ## instead of letting Graphviz choose whatever it wants.  This
    ## helps prevent visual distortion when scaling down the image.

    if(is.null(curAttrs$graph$size)) {
        ## we need to set the size
        ## if a device is open, use that size, otherwise default to 7,7
        if (.Device != "null device"){
            cutAttrs <- replaceAtt(curAttrs, "graph", "size",
                                   paste(par("fin")[1], par("fin")[2], sep= ","))
        } else {
            curAttrs <- replaceAtt(curAttrs, "graph", "size", "6.99,6.99")
        }
    }

    ## Now do layout specific graph attributes
    if (layoutType == "dot")
        curAttrs <- replaceAtt(curAttrs, "graph", "rankdir", "TB")
     

    ## Now do cluster attributes
    curAttrs <- replaceAtt(curAttrs, "cluster", "bgcolor", bg)
    curAttrs <- replaceAtt(curAttrs, "cluster", "color", col)
    curAttrs <- replaceAtt(curAttrs, "cluster", "rank", "same")
    
   
    ## node attributes
    curAttrs <- replaceAtt(curAttrs, "node", "shape", "circle")
    curAttrs <- replaceAtt(curAttrs, "node", "fixedsize", TRUE)
    curAttrs <- replaceAtt(curAttrs, "node", "fillcolor", bg)
    curAttrs <- replaceAtt(curAttrs, "node", "label", "\\N")
    curAttrs <- replaceAtt(curAttrs, "node", "color", col)
    curAttrs <- replaceAtt(curAttrs, "node", "fontcolor", fg)
    curAttrs <- replaceAtt(curAttrs, "node", "fontsize", "14")
    curAttrs <- replaceAtt(curAttrs, "node", "height", "0.5")
    curAttrs <- replaceAtt(curAttrs, "node", "width", "0.75")
    curAttrs <- replaceAtt(curAttrs, "node", "border.lwd", 1)
    curAttrs <- replaceAtt(curAttrs, "node", "border.color", 'black')
    #curAttrs <- replaceAtt(curAttrs, "node", "style", "solid")
    #curAttrs <- replaceAtt(curAttrs, "node", "distortion", "0.0")
    #curAttrs <- replaceAtt(curAttrs, "node", "layer", "solid")
    #curAttrs <- replaceAtt(curAttrs, "node", "regular", "0.0")
    #curAttrs <- replaceAtt(curAttrs, "node", "sides", "4")
    #curAttrs <- replaceAtt(curAttrs, "node", "skew", "0.0")
   

   
    ## edge attrs
    curAttrs <- replaceAtt(curAttrs, "edge", "color", col)
    curAttrs <- replaceAtt(curAttrs, "edge", "dir", "none")
    curAttrs <- replaceAtt(curAttrs, "edge", "weight", "1.0")
    curAttrs <- replaceAtt(curAttrs, "edge", "label", "")
    curAttrs <- replaceAtt(curAttrs, "edge", "fontcolor", fg)
    curAttrs <- replaceAtt(curAttrs, "edge", "arrowhead", "none")
    curAttrs <- replaceAtt(curAttrs, "edge", "arrowtail", "none")
    curAttrs <- replaceAtt(curAttrs, "edge", "fontsize", "14")
    curAttrs <- replaceAtt(curAttrs, "edge", "labelfontsize", "11")
    curAttrs <- replaceAtt(curAttrs, "edge", "arrowsize", "1")
    curAttrs <- replaceAtt(curAttrs, "edge", "headport", "center")
    curAttrs <- replaceAtt(curAttrs, "edge", "layer", "")
    curAttrs <- replaceAtt(curAttrs, "edge", "style", "solid")
   
   
    ## Now do layout specific edge attributes
    if (layoutType == "dot") {
        #curAttrs <- replaceAtt(curAttrs, "edge", "constraint", FALSE)
        curAttrs <- replaceAtt(curAttrs, "edge", "minlen", "1")
    }

    if (layoutType == "neato")
        curAttrs <- replaceAtt(curAttrs, "edge", "len", "1.0")
       

    return(curAttrs)
}


checkAttrs <- function(attrs) {
    if (!is.list(attrs))
        stop("attrs must be a list")
    if (length(attrs) != 4)
        stop("attrs must be of length 4")
    if (!all(names(attrs) %in%
             c("graph","cluster", "node","edge")))
        stop(paste("Names of attrs must be 'graph',",
                   "'cluster', 'node', and 'edge'"))
    TRUE
}
