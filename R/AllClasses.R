## Class xyPoint
setClass("xyPoint", representation(x="numeric", y="numeric"))


## Class AgTextLabel
## used to represent a 'textlabel_t' and related information
setClass("AgTextLabel", representation(labelText="character",
                                       labelLoc="xyPoint",
                                       labelJust="character",
                                       labelWidth="integer",
                                       labelColor="character",
                                       labelFontsize="numeric"))

## Class boundingBox
setClass("boundingBox", representation(botLeft="xyPoint", upRight="xyPoint"))


## Class AgNode
setClass("AgNode", representation(center="xyPoint",
                                  name="character",
                                  txtLabel="AgTextLabel",
                                  height="integer",
                                  rWidth="integer",
                                  lWidth="integer",
                                  color="character",
                                  fillcolor="character",
                                  shape="character",
                                  style="character"))


### Class AgEdge
setClass("AgEdge", representation(splines="list",
                                  sp="xyPoint",
                                  ep="xyPoint",
                                  head="character",
                                  tail="character",
			          dir="character",
                                  arrowhead="character",
                                  arrowtail="character",
                                  arrowsize="character",
                                  color="character",
                                  lty="character",
                                  lwd="numeric",
                                  txtLabel="AgTextLabel"))


### Class BezierCurve
setClass("BezierCurve", representation(cPoints="list"))


### Class Ragraph
setClass("Ragraph", representation(agraph="externalptr",
                                   laidout="logical",
                                   layoutType="character",
                                   edgemode="character",
                                   AgNode="list",
                                   AgEdge="list",
                                   boundBox="boundingBox",
				   fg="character",
				   bg="character"))


##some classes for plotting values of nodes and edges
setClass("pNode", representation(name="character",
                                 attrs="list",
                                 subG="integer"),
         prototype=list(subG=as.integer(0)))


##end type can be arrow or -| or none
##which can be from, to or both
setClass("pEdge", representation(from="character",
                                 to="character",
                                 attrs="list",
                                 subG="integer"),
         prototype=list(subG=as.integer(0))
)
