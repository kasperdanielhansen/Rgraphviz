##Copyright R. Gentleman 2003, all rights reserved


##some classes for plotting values of nodes and edges

setClass("pNode", representation( name="character",
                                 shape="character",
                                 color="character"
                                 ),
         prototype=list(name="", shape="circle", color="white")
         )


##end type can be arrow or -| or none
##which can be from, to or both
setClass("pEdge", representation(from="integer",
                                 to="integer",
                                 style="character",
                                 color="character",
                                 label="character",
                                 arrowhead="character",
                                 dir="character",
                                 fontname="character",
                                 fontcolor="character",
                                 headclip="logical",
                                 headport="character",
                                 tailclip="logical",
                                 weight="numeric"
                                 )
)

