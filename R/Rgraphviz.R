agopen <- function(graph,  name, nodes, edges, kind=NULL,
                   layout=TRUE,	layoutType=c("dot","neato","twopi","circo","fdp"),

                   attrs=list(),
                   nodeAttrs=list(), edgeAttrs=list(),
                   subGList=list(), edgeMode=edgemode(graph),
                   recipEdges=c("combined", "distinct")) {

    layoutType <- match.arg(layoutType)
    recipEdges <- match.arg(recipEdges)
    attrs <- getDefaultAttrs(attrs, layoutType)
    checkAttrs(attrs)

    if (missing(graph) && missing(edgeMode))
        stop("Must pass in either 'graph' or 'edgeMode'")
    if (missing(nodes) && missing(graph))
        stop("Must supply either parameter 'graph' or 'nodes'")
    if (missing(edges) && missing(graph))
        stop("Must supply either parameter 'graph' or 'edges'")

    ## FIXME: For now, in graphviz 2.4 and 2.6 on a neato graph w/
    ##  singleton nodes, it will segfault.  The root cause
    ##  of this is in Graphviz proper and has been fixed in
    ##  the 2.5 devel branch (and thus 2.6).  Try and work
    ##  around this in a less hassling manner.
    ## FIXME: This seems to still be around in 2.6
    if ((graphvizVersion() %in% c("2.4","2.6")) && (layoutType == "neato")) {
        singletonGraph <- any(sapply(connComp(graph), length)<=1)
        if (singletonGraph)
            stop("There is a bad interaction between ",
                 "Rgraphviz and Graphviz 2.4 and 2.6 involving ",
                 "graphs with singleton nodes laid out with neato.\n",
                 "Hopefully we can find a solution, ",
                 "but until then you can ",
                 "use Graphviz versions earlier than 2.4.")
    }

    if (missing(nodes)) {
        nodes <- buildNodeList(graph, nodeAttrs, subGList, attrs$node)
    }
    if (missing(edges)) {
        edges <- buildEdgeList(graph, recipEdges, edgeAttrs,  subGList, attrs$edge)
    }

    if (is.null(kind)) {
        ## Determine kind from the graph object
        outK <- switch(edgeMode,
                       "undirected"=0,  ## AGRAPH
                       "directed"=1,    ## AGDIGRAPH
                       0)
    }
    else {
        ## Use the specified 'kind' parameter.
        outK <- switch(kind,
                       "AGRAPH"=0,   ##Undirected Graph
                       "AGDIGRAPH"=1,   ## directed graph
                       "AGRAPHSTRICT"=2,   ## no self arcs or multiedges
                       "AGDIGRAPHSTRICT"=3, ## directed strict graph
                       stop(paste("Incorrect kind parameter:",kind)))
    }

    g <- .Call("Rgraphviz_agopen", as.character(name),
               as.integer(outK), as.list(nodes),
               as.list(edges), as.list(attrs),
               as.list(subGList), PACKAGE="Rgraphviz")
    g@layoutType <- layoutType
    g@edgemode <- edgeMode

    ## Allows lwd (line width) and lty (line type) to be set in same manner
    ## color is set
    if (layout) g <- layoutGraph(g)
    
    if (!is.null(edgeAttrs$lwd)) {
        for (i in seq(along=edgeAttrs$lwd)) {   
            attr(attr(g, "AgEdge")[[i]], "lwd") <- edgeAttrs$lwd[i]
        }
    }
        
    if (!is.null(edgeAttrs$lty)) {
        for (i in seq(along=edgeAttrs$lty)) {   
            attr(attr(g, "AgEdge")[[i]], "lty") <- edgeAttrs$lty[i]
        }
    }

    return(g)
}

agread <- function(filename, layoutType=c("dot","neato","twopi","circo","fdp"),
                   layout=TRUE	) {
    layoutType <- match.arg(layoutType)
    ## First check that the file exists
    if (!file.exists(filename))
        stop(paste("Request file",filename,"does not exist"))

    g <- .Call("Rgraphviz_agread", as.character(filename), PACKAGE="Rgraphviz")
    g@layoutType <- layoutType

    if (layout)
        return(layoutGraph(g))
    else
        return(g)
}

agwrite <- function(graph, filename) {
    g <- .Call("Rgraphviz_agwrite", graph, as.character(filename),
               PACKAGE="Rgraphviz")
}

layoutGraph <- function(graph, layoutType=c("dot","neato","twopi","circo","fdp")) {
    if (is(graph,"graphNEL"))
        stop("Please use function agopen() for graphNEL objects")
    if (!is(graph,"Ragraph"))
        stop("Object is not of class Ragraph")

    layoutType <- match.arg(layoutType)
    type <- switch(layoutType,
                   "dot"=0,
                   "neato"=1,
                   "twopi"=2,
                   "circo"=3,
                   "fdp"=4,
                   stop(paste("Invalid layout type:",layoutType))
                   )

     z <- .Call("Rgraphviz_doLayout", graph, as.integer(type),
                   PACKAGE="Rgraphviz");
     return(z)
}

graphvizVersion <- function() {
    z <- .Call("Rgraphviz_graphvizVersion", PACKAGE="Rgraphviz")
    z
}

buildNodeList <- function(graph, nodeAttrs=list(), subGList=list(),
                          defAttrs=list()) {
    .Call("Rgraphviz_buildNodeList", nodes(graph), nodeAttrs,
                          subGList, defAttrs, PACKAGE="Rgraphviz")
}

#Katz, May 2005, more fixes to come later
fne <- function(el) {
    ## If there's some sort of empty element,
    ## return a list with an empty numeric vector
    ## so as to properly work w/ the C code
    if (length(el) == 0)
        list(edges=numeric())
    else
        el
}

buildEdgeList <- function(graph, recipEdges=c("combined", "distinct"),
                          edgeAttrs=list(), subGList=list(), defAttrs=list()) {
    recipEdges <- match.arg(recipEdges)

    edgeNames <- edgeNames(graph, "distinct")

    if ((recipEdges == "combined")&&(length(edgeNames) > 0))
        removed <- which(! edgeNames %in% edgeNames(graph, "combined"))
    else
        removed <- character()

    aa <- edgeL(graph)
    aa<-lapply(aa,fne)

    ## Generate the list of pEdge objects
    .Call("Rgraphviz_buildEdgeList", aa, edgemode(graph),
          subGList, edgeNames, removed, edgeAttrs, defAttrs,
          PACKAGE="Rgraphviz")
}

getNodeNames <- function(object) {
    if (!is(object, "Ragraph")) stop("Need a Ragraph object")
    unlist(lapply(object@AgNode, name))
}

getNodeLabels <- function(object) {
    if (!is(object, "Ragraph")) stop("Need a Ragraph object")
    unlist(lapply(object@AgNode, function(x) labelText(x@txtLabel)))
}

removedEdges <- function(graph) {
    if ((!is(graph, "graph")) && (!is(graph,"Ragraph")))
        stop("removedEdges only accepts objects of class ",
             "'graph' or 'Ragraph'")

    allEdges <- edgeNames(graph, "distinct")
    combEdges <- edgeNames(graph, "combined")
    which(! allEdges %in% combEdges)
}

