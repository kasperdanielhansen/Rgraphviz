
## we import the generic from geneplotter, which we depend on

setMethod("imageMap",
  signature=signature(object="Ragraph", con="connection", tags="list",
    imgname="character"),
  definition=function(object, con, tags, imgname, width, height, usr = par("usr")) {

  if(any(par("mai")!=0))
    warning("If par('mai') are not all 0, the result of this function (imageMap) may not be useful.")

  nn = sapply(AgNode(object), function(x) x@name)

  checkTags = function(x, tagname) {
    if(is.null(names(x))) {
      if(length(x)==length(nn)) {
        names(x)=nn
      } else {
        stop(paste("'tags$", tagname, "' must have names if it is not the ",
                   "same length as the number of nodes in 'object'.", sep=""))
      }
    } else {
      if(!all(names(x) %in% nn))
        stop(paste("'names(tags$", tagname, ")' must match the names of ",
                   "the nodes in 'object'", sep=""))
    }
    return(x)
  }

  for(i in seq(along=tags))
    tags[[i]] = checkTags(tags[[i]], names(tags)[i])

  if( !is.numeric(width) ||  length(width)!=1 )
    stop("'width' must be numeric of length 1.")
  if( !is.numeric(height) || length(height)!=1 )
    stop("'height' must be numeric of length 1.")
  if( !is.numeric(usr) || length(usr)!=4 )
    stop("'usr' must be numeric of length 4.")
  
  ## transform user to pixel coordinates
  x.u2p = function(x) { rx=(x-usr[1])/diff(usr[1:2]); stopifnot(all(rx>=0&rx<=1)); return(rx*width)  }
  y.u2p = function(y) { ry=(usr[4]-y)/diff(usr[3:4]); stopifnot(all(ry>=0&ry<=1)); return(ry*height) }

  nxy = getNodeXY(object)
  nh  = getNodeHeight(object)/2
  xl  =   floor(x.u2p( nxy$x - getNodeLW(object) ))
  xr  = ceiling(x.u2p( nxy$x + getNodeRW(object) ))
  yu  =   floor(y.u2p( nxy$y - nh ))
  yl  = ceiling(y.u2p( nxy$y + nh ))
  names(xl) = names(xr) = names(yu) = names(yl) = nn

  mapname <- paste("map", gsub(" |/|#|:", "_", imgname), sep="_")
  base::writeLines(paste("<IMG SRC=\"", imgname, "\" USEMAP=#", mapname, " BORDER=0>",
                   "<MAP NAME=\"", mapname, "\">", sep=""), con)
  for(nd in unique(unlist(lapply(tags, names)))) {
    out = paste("<AREA SHAPE=\"rect\" COORDS=\"", xl[nd], ",", yl[nd], ",",
                xr[nd], ",", yu[nd], "\"", sep="")
    for(i in seq(along=tags))
      if(nd %in% names(tags[[i]]))
        out = paste(out, " ", names(tags)[i], "=\"", tags[[i]][nd], "\"", sep="")
    out = paste(out, ">", sep="")
    base::writeLines(out, con)
  }
  base::writeLines("</MAP>", con)
} ## end of definition
) ## end of setMethod

