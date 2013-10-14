
## Plot a protein interaction graph with edge types
## (c) Wolfgang Huber 7 Mar 2005


library("Rgraphviz")

if(!exists("ft"))
  load("droso_ppi.rda")  ## see droso_ppi_prepare.R

output = c("png", "x11")[1]


## are there any multiple edges?
ftm = as.matrix(ft[,1:2])
ed  = apply(ftm, 1, paste, collapse="---")
dup = duplicated(ed)
cat(sum(dup), "edges are duplicated.\n")
ftm = ftm[!dup,]

## make graphNEL
g = ftM2graphNEL(ftm)

## layout
colors = c("#FFFFB3", "#FDB462") 
ag = agopen(g, name="", nodeAttrs=makeNodeAttrs(g,
                          fillcolor=colors[1+rnai[nodes(g), "Annotation..known.unknown."]]))

## now edit the edges
ed = AgEdge(ag)
for(i in seq(along=ed)) {
  hd = ed[[i]]@head
  tl = ed[[i]]@tail
  wh = which(((ft[,1]==hd) & (ft[,2]==tl)) | (ft[,2]==hd) & (ft[,1]==tl))
  cat(hd, tl, ":", wh, " --- ", ft$DIRECTION[wh], ft[wh, 7], "\n")
  if("BOTH" %in% ft$DIRECTION[wh])
    ed[[i]]@arrowtail = "open"

  iatyp = unique(ft[wh, 7])
  lwd = 2
  switch(iatyp[1],
         Enhancement = { col= "#E41A1C"; lty=1 },
         Suppression = { col= "#377EB8"; lty=1 },
         "Two Hybrid (High Confidence)" = { col= "#8DA0CB"; lty=1 },
         "Two Hybrid" = { col= "#8DA0CB"; lwd=1; lty=2 },
         stop(iatyp[1]))
  if(all(c("Enhancement", "Suppression")%in% iatyp))
    col="black"
  
  ed[[i]]@lwd=lwd
  ed[[i]]@lty=lty
  ed[[i]]@color=col
}
AgEdge(ag)=ed


switch(output,
       png = {
         fhtml = "droso_ppi.html"
         fpng  = "droso_ppi.png"
         width = height = 800
         png(fpng, width=width, height=height)
       },
       x11 = {}
       )

par(mai=rep(0,4))
plot(ag)

switch(output,
       png = {
         title = nodes(g)
         url = paste("http://www.ensembl.org/Drosophila_melanogaster/textview?idx=All&q=",
           nodes(g), "&x=0&y=0", sep="")
         ## Write an HTML file with the image map
         con = file(fhtml, open="wt")
         writeLines("<html><head><title>Klick mich!</title></head><body>\n", con)

         imageMap(ag, con, fpng, tags=list(HREF=url, TITLE=title), width=width, height=height)

         writeLines(c('<h3>Legend</h3><font color="#377EB8">blue: Suppression</font><br>',
                      '<font color="#E41A1C">red: Enhancement</font><br>',
                      '<font color="black">black: both!</font><br>',
                      '<font color="#8DA0CB">grey: Y2H (solid for "high confidence")<br>'), con=con)
 

         writeLines("</body></html>", con)
         dev.off()
         close(con)
         cat("Now open the file droso_ppi.html with your browser.\n")
       },
       x11 = {}
       )

