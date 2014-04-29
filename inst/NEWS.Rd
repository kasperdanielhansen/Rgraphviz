\name{cqnnews}
\title{Rgraphviz News}
\encoding{UTF-8}

\section{Version 2.9.x}{
  \itemize{
    \item Fixed a bug in the C code of Graphviz which manifests itself
    when the following options are given to GCC
    "-Wformat -Wformat-security -Werror=format-security -D_FORTIFY_SOURCE=2",
    this is the case for Ubuntu 12.04.
    Reported by Vladimir Zhurov <vzhurov2@uwo.ca>, Venkat Seshan
    <veseshan@gmail.com> and Chong Tang <tangc3@unr.edu>.
  }
}

\section{Version 2.7.x}{
  \itemize{
    \item Fixed include statements in Graphviz code.
  }
}


\section{Version 2.5.x}{
  \itemize{
    \item Expose mai argument in plot,Ragraph-method.
    \item Changed the LICENSE to EPL.
    \item Fixed some wrong text in the DESCRIPTION file.
    \item Fixed pieGylph to adress the issue of warning messages about
    rep(NULL) (reported by Cristobal Fresno Rodríguez
    <Cristobalfresno@gmail.com>).
    \item Updated Imports, Depends, NAMESPACE as per R CMD check
    requirements.
    \item Fixing issue with the lt~obsolete.m4 file(s) in Graphviz; R CMD
    check was issueing a warning.
    \item Moved vignettes from inst/doc to vignettes directory.
  }
}

\section{Version 2.4.x}{
  \itemize{
    \item Backported the pieGylph fix to the release branch.
  }
}

\section{Version 2.3.x}{
  \itemize{
    \item Fixing bug in Graphviz on OpenBSD regarding sincos (Thanks to
    Rainer Hurling).
    \item Now using path.expand to fix tilde-expansion.
    \item We no longer include <R_ext/RConverters.h>; which seemingly
    was not needed (Thanks to Brian D. Ripley).
    \item Fix handling of fontsize attrib for nodes and edges (seems it
    has been broken at least since 2006).
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
