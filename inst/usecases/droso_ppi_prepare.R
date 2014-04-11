
## This script prepares the data object droso_ppi.rda which will be needed
## by droso_ppi.R
## (c) Wolfgang Huber 7 Mar 2005


options(error=recover)

if(!exists("ia")) {
  ## A file with curated protein protein interactions that was downloaded from
  ## Flygrid database:
  ia   = read.table("flygrid_interactions.txt",
    header=TRUE, skip=32, quote="", as.is=TRUE, sep="\t", comment.char="") 
  stopifnot(nrow(ia)== 26596, ncol(ia)==8)
  
  ## A list of genes from an RNAi screen:
  rnai = read.table("rnai3.txt", as.is=TRUE, header=TRUE, sep="\t")
  rnai$Gene = tolower(rnai$Gene)
  stopifnot(!is.null(rnai$Gene), !any(duplicated(rnai$Gene)))
  rownames(rnai) = rnai$Gene
}

## This function takes a character vector 's' whose elements are gene
## or protein names. If one of them matches an element of 'rnai',
## the index of the match is returned; otherwise, 0.
matchfun = function(s) {
  s = unique(tolower(s[s!=""]))
  m  = match(s, rnai$Gene)
  wh = which(!is.na(m))
  if(length(wh)>1) {
    warning(paste("Interaction partner ", paste(s, collapse="="), " hits multiple genes in RNAi list: ",
                  paste(rnai$Gene[m[wh]], collapse=", "), sep=""))
    wh = wh[1] ## just take the first!
  }
  rv = 0
  if(length(wh)>0)
    rv = m[wh]
  return(rv)
}

if(!exists("select")) {
  ## split at ';'
  pr1 = strsplit(ia$"ALIASES_FOR_A", ";", fixed=TRUE)
  pr2 = strsplit(ia$"ALIASES_FOR_B", ";", fixed=TRUE)
  ## and try to match with rnai list
  mt1 = sapply(pr1, matchfun)
  mt2 = sapply(pr2, matchfun)
  ## the interactions that had both interaction partners in the rnai list
  select = (mt1>0) & (mt2>0)
}

##----------------------------------------------------------------
## non-hits (genes in RNAi list that have no interactions at all
##----------------------------------------------------------------
hits = unique(c(mt1, mt2))
hits = setdiff(hits, 0)
nonhits = sort(rnai$Gene[-hits])
print(nonhits)

library("graph")
ft = cbind(rnai$Gene[mt1[select]], rnai$Gene[mt2[select]], ia[select, ])

save(ft, rnai, file="droso_ppi.rda", compress=TRUE)
