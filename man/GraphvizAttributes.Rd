\name{GraphvizAttributes}
\alias{GraphvizAttributes}
\alias{graphviz}
\alias{edgeAttributes}
\alias{nodeAttributes}
\alias{graphAttributes}
\title{Graph Attributes for Rgraphviz}
\description{
  The following describes the allowable attributes to be used with
  Rgraphviz.  Most of these are sent directly to Graphviz and will
  influence the plot layout, some of these are only cosmetic and stay in
  R.  Users are referred to the Graphviz web documentation which
  contains some more detailed information on these attributes (and is
  our source).
}
\section{Graph Attributes}{
  \bold{General Graph Attributes:}
  \describe{
    \item{\code{bgcolor}:}{Color for the entire canvas.  The default is
      to use \code{transparent}} 
    \item{\code{center}:}{Drawing is centered in the output canvas.
      This is a boolean value with a default of \code{FALSE}.}
    \item{\code{fontcolor}:}{Color used for text, defaulting to
      \code{black}.}
    \item{\code{fontname}:}{Font used for text.  Default is Times Roman.}
    \item{\code{fontpath}:}{Directory used to search for fonts}
    \item{\code{fontsize}:}{Font size, in points, for text.  This
      attribute accepts doubles for values with a default of \code{14.0}
      and a minimum value of \code{1.0}}
    \item{\code{label}:}{Label for the graph.  This is a string with
      some extra escape sequences which can be used.  The substring
      \code{\\N} is replaced by the name of the node, and the substring
      \code{\\G} by the name of the graph.  For graph or cluster
      attributes, \code{\\G} is replaced by the name of the graph or
      cluster.  For edge attributes, the substring \code{\\N} is
      replaced by the name of the edge, and the substrings \code{\\T}
      and \code{\\H} by the names of the tail and head nodes.  The
      default value for nodes is \code{\\N} with everything else
      defaulting to \code{}}
    \item{\code{labeljust}:}{Subgraphs inherit cluster behavior for
      labeljust.  If \code{r}, the labels are right-justified within
	the bounding rectangle.  If \code{l} they are left-justified.
	Otherwise the labels are centered.  The default value is
	\code{c}.} 
    \item{\code{labelloc}:}{Top/bottom placement for graph labels.  A
      value of \code{t} places the label at the top, \code{b} places
      them at the bottom.  By default root graph labels go on the bottom
      and cluster labels go on the top.}
    \item{\code{layers}:}{Specifies a linearly ordered list of layer
      names attached to the graph.  Only those components belonging to
      the current layer appear.  This attribute accepts a
      \code{layerList}, which is list of strings separated by the
      characters defined with the \code{layersep} attribute which
      defines layer names and implicitly numbered 1,2,etc.  The default
      is \code{} which only uses one layer.}
    \item{\code{layersep}:}{Separator characters used to split the
      layers into a list of names.  The default is colons, tabs or
      spaces.} 
    \item{\code{margin}:}{Set X&Y margins of canvas in inches.
      Specified using the notation \code{val,val} where each val is a
      double.} 
    \item{\code{orientation}:}{If "[1L*]*", set graph orientation to
      landscape.  Used only if \code{rotate} is not defined.  Default is
      the empty string.}
    \item{\code{page}:}{Width & height of output pages, in inches.  If set
	and smaller then the size of the layout, a rectangular
	array of pages of the specified page size is overlaid on the
	layout, with origins aligned in the lower-left corner, thereby
	partitioning the layout into pages.  The pages are then produced
	one at a time in 'pagedir' order.  Specified using the notion
	\code{val,val} where each val is a double.}
    \item{\code{pagedir}:}{If the \code{page} attribute is set and
      applicable, this attribute specifies the order in which the pages
      are emitted.  This is limited to one of the 8 row or column major
      orders.  One of \code{BL}, \code{BR}, \code{TR}, \code{RB},
      \code{RT}, \code{LB}, or \code{LT} specifying the 8 row or column
      major orders for traversing a rectangular array (the first
      character corresponding to the major order and the second to the
      minor order).  The default value is \code{BL}}
    \item{\code{quantum}:}{If 'quantum' > 0.0, node label dimensions
      will be rounded to integral multiples of the quantum.  This
      attribute is of type double, with a default and minimum value of
      \code{0.0}} 
    \item{\code{ranksep}:}{In dot, this gives the desired rank
      separation in inches.  If the value contains \code{equally}, the
      centers of all the ranks are spaced equally apart.  In twopi,
      specifies the separation of concentric circles.  This value is of
    type double, with a minimum value of \code{0.02}.  In dot, the
    default value is \code{0.5} and for twopi it is \code{1.0}.} 
    \item{\code{ratio}:}{Sets the aspect ratio for the drawing.  If
      numeric, defines aspect ratio.  If it is 'fill' and 'size'
      has been set, node positions are scaled such that the final
      drawing fills exactly the specified size.  If 'compress' and the
      'size' attribute is set and the graph can not be drawn on a single
      page, then the drawing is compressed to fit in the given size.  If
      'auto', the 'page' attribute is set and the graph cannot be drawn
      on a single page, then 'size' is set to an \dQuote{ideal} value.  The
      default for this attribute is \dQuote{fill}}
    \item{\code{rotate}:}{If 90, set drawing orientation to landscape.
      This attribute accepts integer values and defaults to \code{0}}
    \item{\code{samplepoints}:}{If the input graph defines the
      'vertices' attribute, and output is dot or xdot, this gives the
      number of points used to represent circles and ellipses.  It plays
      the same role in neato, when adjusting the layout to avoid
      overlapping nodes.  This attribute accepts integer values and
      defaults to \code{8}} 
    \item{\code{size}:}{Maximum width and height of drawing, in inches.
      Specified using the notation \code{val,val} where each val is a
      double.  The default of this attribute is \code{6,6}.} 
    \item{\code{start}:}{Parameter used to determine the initial layout
      of nodes.  By default, nodes are randomly placed in a
      square whose sides have length (number of nodes).  The same seed
      is always
      used for the random number generator, so the initial placement is
      repeatable.  If 'start' converts to an integer, this is used as a
      seed value for the RNG.  If 'start' is "regular", the nodes are
      placed regularly about a circle.  Finally if 'start' is defined,
      and not one of those cases, the current time is used to pick a
      seed.  The default is the empty string.}
  }
  \bold{Dot Only Attributes}
  \describe{
    \item{\code{clusterrank}:}{Mode used for handling
      clusters.  If "local", a subgraph whose name begins with "cluster"
      is given special treatment.  The subgraph is laid out separately,
      and then  integrated as a unit into its parent graph.  If the cluster has
      a 'label' parameter, this label is displayed w/ the cluster.
      Acceptable values are \code{local}, \code{global} or \code{none} -
    with \code{local} being the default.} 
    \item{\code{compound}:}{Allow edges between clusters.  This is a
      boolean value with a default of \code{FALSE}}
    \item{\code{concentrate}:}{Use edge concentrators.  This is a
      boolean value with a default of \code{FALSE}}
    \item{\code{mclimit}:}{Scale factor used to alter the 'MinQuit' and
      'MaxIter' parameters used during crossing minimization.  This is a
    double value with a default of \code{1.0}}
    \item{\code{nodesep}:}{Minimum space between two adjacent nodes of
      the same rank, in inches.  This is a double value with a default
      of \code{0.25} and a minimum of \code{0.02}}
    \item{\code{ordering}:}{If \code{out} for a graph and n is a node in
      G, then edges n->* appear left to right in the order that they
      were defined.}
    \item{\code{rankdir}:}{Determines if the layout is left-to-right or
      top-to-bottom.  Acceptable values are \code{LR} and \code{TB} with
    the default being the latter.}
    \item{\code{remincross}:}{If \code{TRUE} and there are multiple
      clusters, run cross minimization a second time.  Default is \code{FALSE}}
    \item{\code{searchsize}:}{During network simplex, maximum number of edges
      with negative cut values to search when looking for one with
      minimum cut value.  This is an integer value with a default of \code{30}}
    \item{\code{showboxes}:}{Debugging feature for postscript output, R
      only.  Not currently implemented.}
  }
  \bold{Neato Only Attributes}
  \describe{
    \item{\code{Damping}:}{Factor damping force motions.  On each
      iteration, a node's movement is limited to this factor of its
      potential motion.  This is a double value with a default of
      \code{0.99} and a minimum value of \code{0.0}}
    \item{\code{defaultdist}:}{Default distance between nodes in
      separate connected components.  Only applicable if
      \code{pack=FALSE}.  Defaults to \code{1+(avg. len)*sqrt(|V|)} and
      has a minimum value of \code{epsilon}.}
    \item{\code{dim}:}{Set the number of dimensions used for the
      layout.  This is an integer value with a default and minimum value
    of \code{2}.}
    \item{\code{epsilon}:}{Terminating condition.  If length squared of
      all energy gradients are less than epsilon, the algorithm stops.
      This is a double value with a default of \code{.0001 times the
	number of nodes}.} 
    \item{\code{maxiter}:}{Sets the number of iterations used.  This is
      an integer value with the default of \code{MAXINT}.}
    \item{\code{model}:}{If \code{circuit}, use circuit resistance model
      to compute dissimilarity values, otherwise use shortest path.
      Defaults to the empty string.}
  }
  \bold{Twopi Only Attributes}
  \describe{
    \item{\code{root}:}{Name of the node to use as the center of the
      layout.  If not defined, will pick the most central node.}
  }
  \bold{Not Dot Attributes}
  \describe{
    \item{\code{normalize}:}{Normalize coordinates of final layout so
      that the first point is at the origin, then rotate so that the
      first edge is horizontal.  This is a boolean value with a default
      of \code{FALSE}.}
    \item{\code{overlap}:}{If \code{scale}, remove node overlaps by
      scaling.  If \code{FALSE}, use Voronoi technique, otherwise leave
      overlaps.  Default is the empty string.}
    \item{\code{pack}:}{If "true" or non-negative integer - each
	connected component is laid out separately and then the graphs
	are packed tightly.  If pack has an integral value, this is used
	as the size (in points) of a margin around each part; otherwise
	a default margin of 8 is used. If 'false', the entire graph is
	laid out together.  For twopi layouts, this just sets the
	margin.  Default is \code{FALSE}}
    \item{\code{packmode}:}{Indicates the granularity and method used
      for packing.  This will automatically turn on \code{pack}.
      Acceptable values are \code{node}, \code{clust} and \code{graph},
      specifying the granularity of packing connected components when
      \code{pack=TRUE}.  The default is \code{node}.}
    \item{\code{sep}:}{Fraction to increase polygons for purposes of
      determining overlap.  This is a double value with a default of \code{0.01}.}
    \item{\code{splines}:}{Draw edges as splines.  This is a boolean
      value with a default of \code{TRUE}.}
    \item{\code{voro_margin}:}{Factor to scale up drawing to allow
      margin for expansion in Voronoi technique.  This is a double value
    with a default of \code{0.05} and a minimum value of \code{0.0}} 
  }
  \bold{Output Based}
  \describe{
    \item{\code{resolution}:}{Number of pixels per inch on a display,
      used for SVG output.  The default for this attribute is
      \code{0.96} and it accepts values of type double.}
    \item{\code{stylesheet}:}{A URL or pathname specifying a XML style
      sheet, for SVG output.}
    \item{\code{truecolor}:}{Output relies on a truecolor color model,
      used with bitmap outputs.  This is a boolean value with a default
      of \code{FALSE}}
  }
  \bold{Misc}
  \describe{
    \item{\code{URL}:}{Hyperlink incorporated into the output.  For
      whole graphs, used as part of an image map.  Not currently implemented}
    \item{\code{comment}:}{A device dependent commentary.  Not currently
    implemented.}
    \item{\code{nslimit}:}{Number of iterations in network simplex
      applications.  Used in computing node x coordinates}
    \item{\code{nslimit1}:}{Same as \code{nslimit} but for ranking
      nodes}
    \item{\code{outputorder}:}{Specify order in which nodes and edges are
    drawn.  R only}
  }
}
\section{Edge Attributes}{
  \bold{General Edge Attributes}
  \describe{
    \item{\code{arrowhead}:}{Shape of the arrowhead.  Currently somewhat
      limited in what can be rendered in R as opposed to what is
      available via Graphviz in that only \code{open} and \code{none}
      are allowed.  The \code{open} is used by default for directed
      edges and \code{none} for undirected edges.  R only(?)}
    \item{\code{arrowsize}:}{Multiplicative scale factor for arrowheads,
      type double.  R only (?).  This attribute accepts values of type
      double with a default of \code{1.0} and minimum of \code{1.0}.}
    \item{\code{arrowtail}:}{Style of arrow on the tail node of an edge,
      see \code{arrowhead}.  For directed edges that with bidirectional
      arrows, \code{open} is used.  R only(?)}
    \item{\code{color}:}{The color of the edge.  Defaults to
      \code{black}.} 
    \item{\code{decorate}:}{If TRUE, attach edge label to edge by a
      2-segment polyline, underlining the label, then going to the
      closest point of the spline.  Default is \code{FALSE}.}
    \item{\code{dir}:}{Edge type drawing, which ends should get the
      arrowhead.  R only(?).  For directed graphs, this defaults to
      \code{forward} and undirected graphs default to \code{both}.
      Other possible values are \code{both} (arrows in both
      directions) and \code{back} (Arrow on the tail only)}
    \item{\code{fontcolor}:}{The color used for text.  The
      default value is \code{black} in R}
    \item{\code{fontname}:}{Font used for text.  Defaults to Times Roman}
    \item{\code{fontsize}:}{Font size, used for text.  Defaults to
      \code{14.0} with a minimum value of \code{1.0}}
    \item{\code{headclip}:}{Head of the edge is clipped to the boundary
      of the head node, otherwise it goes to the center of the node.
      This is a boolean value with the default of \code{TRUE}}
    \item{\code{headlabel}:}{Label for the head of the edge.  See
      \code{label} in \code{Graph Attributes} for a description of
      additional escape sequences.}
    \item{\code{headport}:}{Where on the node to aim the edges, uses
      \code{center},
      \code{n}, \code{s}, \code{e}, \code{nw}, \code{nw}, \code{se}, and
      \code{sw}.  The default is \code{center}.}
    \item{\code{label}:}{The edge label.  See
      \code{label} in \code{Graph Attributes} for a description of
      additional escape sequences.}
    \item{\code{labelangle}:}{Angle in degrees that the label is
      rotated, as a double.  Default is \code{-25.0} with a minimum
      value of \code{-180.0}}
    \item{\code{labeldistance}:}{Multiplicative scaling factor adjusting
      the distance that the label is from the node.  This is a double
      value with a default of \code{1.0} and a minimum value of \code{0.0}.}
    \item{\code{labelfontcolor}:}{Color used for the edge labels.  This
      defaults to \code{black}} 
    \item{\code{labelfontname}:}{Font used for the edge labels.  See
      \code{fontname}} 
    \item{\code{labelfontsize}:}{Size used for the edge labels.  See
      \code{fontsize}. }
    \item{\code{layer}:}{Specifies the layers that this edge is present,
      type of \code{layerRange}.  This is specified as a string in the
      format \code{layerID} or \code{layerIDslayerIDslayerID...}, where
      \code{s} is a character from the \code{layersep} attribute.  The
      \code{layerID} attribute can be \code{all}, a decimal integer or a
    layer name.  Defaults to the empty string.} 
    \item{\code{pos}:}{Position of the spline control points, in points}
    \item{\code{style}:}{Set line style for the edge.  R only.  Can be
      one of \code{dashed}, \code{dotted}, \code{solid}, \code{invis}
      and \code{bold}.  Defaults to \code{solid}.}
    \item{\code{tailclip}:}{Same as \code{headclip} except for the tail
      of the edge.  Defaults to \code{TRUE}}
    \item{\code{taillabel}:}{Same as \code{headlabel} except for the
      tail of the edge.  Defaults to the empty string.}
    \item{\code{weight}:}{The weight of the edge.  This attribute is of
      type double with a default of \code{0.75} and a minimum value of
      \code{0.01}.} 
  }
  \bold{Dot Only Attributes}
  \describe{
    \item{\code{constraint}:}{If FALSE, edge is not used in ranking
      nodes.  Default is \code{TRUE}.}
    \item{\code{lhead}:}{Logical head of an edge.  If \code{compound} is
      TRUE, if \code{lhead} is defined and is the name of a cluster
      containing the real head, then the edge is clipped to the boundary
      of the cluster.}
    \item{\code{ltail}:}{Same as \code{lhead} but for the tail of the
      edge}
    \item{\code{minlen}:}{Minimum edge length (rank difference between
      head and tail).  This is an integer value with a default of
      \code{1} and a minimum of \code{0}.}
    \item{\code{samehead}:}{Edges with the same head and \code{samehead}
      value are aimed at the same point on the head node.}
    \item{\code{sametail}:}{Same as \code{samehead} but for the tail of
      the edge.}
  }
  \bold{Neato Only Attributes}
  \describe{
    \item{\code{len}:}{Preferred edge length, in inches.  This attribute
    accepts double values with a default of \code{1.0}.}
  }
  \bold{Misc}
  \describe{
    \item{\code{URL}:}{Hyperlink incorporated into the output.  Not
      currently supported}
    \item{\code{headURL}:}{URL for the head of the edge.  Not currently supported}
    \item{\code{headtooltip}:}{If there's a headURL, annotation for a
      tooltip.  R only, not currently supported}
    \item{\code{tailURL}:}{Same as \code{headURL} but for the tail of an
      edge}
    \item{\code{tailtooltip}:}{Same as \code{headtooltip} but for the
      head of an edge}
    \item{\code{tooltip}:}{Same as \code{headtooltip} but for the edge
      in general}
    \item{\code{showboxes}:}{Debugging feature for postscript.  Not
      currently supported}
    \item{\code{comment}:}{Device dependent comment inserted into the
      output.  Not currently supported}
  }
}
\section{Node Attributes}{
  \bold{General Node Attributes}
  \describe{
    \item{\code{color}:}{Basic drawing color for the node, corresponding
      to the outside edge of the node.  The interior of the node is
      specified with the \code{fillcolor} attribute. Defaults to
      \code{black}.} 
    \item{\code{distortion}:}{Distortion factor for
      \code{shape=polygon}, positive values cause top to be larger then
      bottom, negative is opposite.  This is a double value with a
      default of \code{0.0} and a minimum value of \code{-100.0}}
    \item{\code{fillcolor}:}{Background color of the node.  This
      defaults to \code{black}.} 
    \item{\code{fixedsize}:}{Use only \code{width} and \code{height}
      attributes, do not expand for the width of the label.  This
      defaults to \code{TRUE}.}
    \item{\code{fontcolor}:}{Color used for text.  This
      defaults to \code{black}.}
    \item{\code{fontname}:}{Font used for text.  The default of this is
      Times Roman.}
    \item{\code{fontsize}:}{Size of font for the text.  This defaults to
    \code{14.0} with a minimum size of \code{1.0}.}
    \item{\code{height}:}{Height of the node, in inches.  This attribute
    accepts values as doubles with a default of \code{0.5} and a minimum
  value of \code{0.02}.} 
    \item{\code{label}:}{Label for the node.  See \code{label} in
      \code{Graph Attributes} for an explanation of extra escape sequences.}
    \item{\code{layer}:}{Layers in which the node is present.  See
      \code{layer} in \code{Edge Attributes} for an explanation of
      acceptable inputs. }
    \item{\code{orientation}:}{Angle, in degrees, used to rotate node
      shapes.  This accepts double values with a default of \code{0.0}
      and a minimum of \code{360.0} (Which doesn't make sense, but going
    off of the Graphviz website noted in \code{references}.}
    \item{\code{peripheries}:}{Set number of peripheries used in
      polygonal shapes and cluster boundaries.  Note that user-defined
      shapes are treated as a form box shape, so the default peripheries
      value is 1 and the user-defined shape will be drawn in a bounding
      rectangle.  Setting peripheries=0 will turn this off.  Also, 1 is
      the maximum peripheries value for clusters.}
    \item{\code{pos}:}{Position of the node (For neato layouts, this is
      the initial position of the node).   Specified using the notion
      \code{val,val} where each val is a double.}
    \item{\code{regular}:}{Force the polygon to be regular.  Defaults to
    \code{FALSE}.} 
    \item{\code{shape}:}{The shape of the node.  Current acceptable
      values are \code{circle}, \code{rectangle}, \code{rect},
      \code{box} and \code{ellipse}.  The \code{circle} shape is the
      default.  Note that \code{box}, \code{rect} and \code{rectangle}
      all correspond to the same actual shape.}
    \item{\code{sides}:}{Number of sides if \code{shape=polygon}.  This
      is an integer value with a default value of \code{4} and a minimum
    value of \code{0}.}
    \item{\code{skew}:}{Skew factor for \code{shape=polygon}.  Positive
      values skew the polygon to the right, negative to the left.  This
      is a double value with a default value of \code{0.0} and a minimum
    value of \code{-100.0}.}
    \item{\code{style}:}{Set style for the node boundary.  R only.  Can be
      one of \code{dashed}, \code{dotted}, \code{solid}, \code{invis}
      and \code{bold}.  Defaults to \code{solid}.}
    \item{\code{width}:}{Width of the node, in inches.  Default is
      \code{0.75} with a minimum value of \code{0.01}}
  }
  \bold{Dot Only Attributes}
  \describe{
    \item{\code{group}:}{If the end points of an edge belong to the same
      group, ie they have the same group attributes, parameters are set
      to avoid crossings and keep the edges straight}
  }
  \bold{Neato Only Attributes}
  \describe{
    \item{\code{pin}:}{If TRUE and node has a \code{pos} attribute on
      input, neato prevents the node from moving from the input
      position.  The default for this attribute is \code{FALSE}.}
  }
  \bold{Misc}
  \describe{
    \item{\code{tooltip}:}{Annotated tooltip if URL exists.  R only.
      Currently unsupported}
    \item{\code{toplabel}:}{Label near the top of nodes of shape M*.
      Currently unsupported}
    \item{\code{URL}:}{Hyperlink incorporated into the output.  Not
      currently supported}
    \item{\code{bottomlabel}:}{Same as \code{toplabel} but for the
      bottom of the node}
    \item{\code{comment}:}{Device dependent comment inserted into the
      output.  Not currently supported}
    \item{\code{shapefill}:}{If non-empty, if output is ps or svg and
      shape is 'espf', taken as a filename containing a device-dependent
      description of a node's shape.  If non-empty for bitmap output
      (gif, jpg, etc), and shape set to 'custom', taken as the URL for a
      file containing the bitmap image for the node.  For files on the
      local machine, the URL begins with "file://".  For remote files,
      graphviz must have been configured to use a command such as curl
      to retrieve the files remotely.  Currently unsupported}
    \item{\code{z}:}{Provides z coordinates for the node in a 3D system.
      Currently unsupported}
  }
}
\section{details}{
   Different attributes are appropriate for different specific graph
   layout algorithms. Graphviz supports three different layout
   algorithms, dot, neato and twopi.

   There is some tension between attributes that graphviz supports and
   those that we can support at the R level. Please let us know if there
   are situations that are not being handled appropriately.
   
   All attributes are passed down to graphviz. However they can be later
   modified for rendering in R.

  }
\references{\url{http://www.research.att.com/~erg/graphviz/info/attrs.html}}
\author{Jeff Gentry}
\seealso{\code{\link{plot.graph}}, \code{\link{agopen}}, \code{\link{GraphvizLayouts}}}
\examples{

}
\keyword{graphs}
