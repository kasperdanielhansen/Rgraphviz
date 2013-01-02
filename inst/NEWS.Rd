\name{cqnnews}
\title{Rgraphviz News}
\encoding{UTF-8}


\section{Version 2.3.x}{
  \itemize{
    \item Now using path.expand to fix tilde-expansion.
    \item We no longer include <R_ext/RConverters.h>; which seemingly
    was not needed (Thanks to Brian D. Ripley).
  }
}

\section{Version 2.2.x}{
  \itemize{
    \item Plotting now longer accepts border.lwd or border.col.  Instead
    of border.col use color.  Instead of border.lwd, you need to use the
    renderGraph/layoutGraph interface.  This fixes a long standing bug
    where Rgraphviz writes illegal dot files.  unitTest has been added.
    \item bugfix to renderNodes (used in renderGraph) for nodes with
    shape = "triangle".  It now works.
    \item bugfix to renderNodes (used in renderGraph) for using
    node-specific lwd and shapes either "triangle", "ellipse" or "circle".
    It now works. 
  }
}

\section{Version 2.1.x}{
  \itemize{
    \item Rgraphviz now requires Graphviz >= 2.16.
    \item Added graphvizCapabilities() that reports the capabilities of
    Graphviz.  This requires Graphviz >= 2.28 and returns NULL if the
    Rgraphviz installation does not support it.
    \item Rgraphviz now comes bundled with Graphviz 2.28, greatly
    simplifying installation.
    \item Numerous bugfixes for bugs introduced in the 2.x.x versions. 
  }
}
